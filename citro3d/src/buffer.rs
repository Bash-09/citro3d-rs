//! Configure vertex buffer objects to be sent to the GPU for rendering.
//!
//! See the [`attrib`] module for details on how to describe the shape and type
//! of the VBO data.

use std::{any::Any, marker::PhantomData, mem::MaybeUninit, pin::Pin};

use ctru::linear::{LinearAllocation, LinearAllocator};

use crate::attrib;

#[derive(Debug)]
#[doc(alias = "C3D_BufInfo")]
pub struct OwnedInfo {
    buffers: Vec<Pin<Box<dyn Any>>>,
    indices: Option<Pin<Box<dyn Any>>>,
    // SAFETY: Not actually static!!
    info: Info<'static>,
}

impl OwnedInfo {
    pub fn new(primitive: Primitive) -> Self {
        OwnedInfo {
            buffers: Vec::new(),
            indices: None,
            info: Info::new(primitive),
        }
    }

    pub fn info<'a>(&'a self) -> &'a Info<'a> {
        &self.info
    }

    pub fn buffer<'a>(&'a self, id: u8) -> Option<Slice<'a>> {
        self.info.buffer(id)
    }

    /// Register vertex buffer object data from an owned Linear allocation.
    /// `vbo_data` is assumed to use one `T` per drawn primitive, and
    /// its layout is assumed to match the given `attrib_info`.
    ///
    /// # Errors
    ///
    /// Registering VBO data may fail:
    ///
    /// * if the maximum number (12) of VBOs are already registered
    /// * if the length of the vbo_data is too long
    pub fn add_linear<T: 'static, C: LinearAllocation + AsRef<[T]> + 'static>(
        &mut self,
        vbo_data: C,
        attrib_info: &attrib::Info,
    ) -> crate::Result<u8> {
        self.buffers.push(Box::pin(vbo_data));
        let container: &C = self.buffers.last().unwrap().downcast_ref().unwrap();

        // SAFETY: The borrowed data is only ever accessed with the lifetime of this struct,
        // not `'static`. Once pinned and added to `self.buffers`, the container is never moved
        // or mutated until it is freed when this struct is dropped.
        let buffer: &'static [T] = unsafe { std::mem::transmute(container.as_ref()) };
        let res = self.info.add(buffer, &attrib_info);

        if res.is_err() {
            self.buffers.pop();
        }

        res
    }

    /// Register vertex buffer object data. If you have an owned linear allocation of the data,
    /// [`add_linear`] is preferred to avoid an additonal reallocation into Linear memory.
    /// `vbo_data` is assumed to use one `T` per drawn primitive, and
    /// its layout is assumed to match the given `attrib_info`.
    ///
    /// # Errors
    ///
    /// Registering VBO data may fail:
    ///
    /// * if the maximum number (12) of VBOs are already registered
    /// * if the length of the vbo_data is too long
    pub fn add<T: Clone + 'static, C: AsRef<[T]>>(
        &mut self,
        vbo_data: C,
        attrib_info: &attrib::Info,
    ) -> crate::Result<u8> {
        let data = vbo_data.as_ref();
        let mut container = Vec::with_capacity_in(data.len(), LinearAllocator);
        container.extend_from_slice(data);

        self.add_linear(container, attrib_info)
    }

    /// Register an owned index buffer allocated in Linear memory.
    /// indices` must contain an array of either [`u8`] or [`u16`] elements.
    ///
    /// # Errors
    ///
    /// Registering VBO data may fail:
    ///
    /// * if the length of the index array is too long
    pub fn add_indices_linear<I: Index + 'static, C: LinearAllocation + AsRef<[I]> + 'static>(
        &mut self,
        indices: C,
    ) -> crate::Result<()> {
        let old = self.indices.replace(Box::pin(indices));
        let container: &C = self.indices.as_ref().unwrap().downcast_ref().unwrap();

        // SAFETY: The borrowed data is only ever accessed with the lifetime of this struct,
        // not `'static`. Once pinned and added to `self.buffers`, the container is never moved
        // or mutated until it is freed when this struct is dropped.
        let buffer: &'static [I] = unsafe { std::mem::transmute(container.as_ref()) };
        let res = self.info.add_indices(buffer);

        if res.is_err() {
            self.indices = old;
        }

        res
    }

    /// Register index buffer data. If you have an owned linear allocation of the data,
    /// [`add_indices_linear`] is preferred to avoid an additional reallocation into Linear memory.
    /// `indices` must contain an array of either [`u8`] or [`u16`] elements.
    ///
    /// # Errors
    ///
    /// Registering VBO data may fail:
    ///
    /// * if the length of the index array is too long
    pub fn add_indices<I: Index + Clone + 'static, C: AsRef<[I]>>(
        &mut self,
        indices: C,
    ) -> crate::Result<()> {
        let data = indices.as_ref();
        let mut container = Vec::with_capacity_in(data.len(), LinearAllocator);
        container.extend_from_slice(data);

        self.add_indices_linear(container)
    }
}

/// Vertex buffer info. This struct is used to describe the shape of the buffer
/// data to be sent to the GPU for rendering.
#[derive(Debug)]
#[doc(alias = "C3D_BufInfo")]
pub struct Info<'buf> {
    pub(crate) info: citro3d_sys::C3D_BufInfo,
    // TODO
    // Only one index array for the whole thing, they are not per-buffer
    pub(crate) lens: [libc::c_int; 12],
    pub(crate) indices: Option<Indices>,
    pub(crate) primitive: Primitive,
    _phantom: PhantomData<&'buf ()>,
}

impl<'buf> Info<'buf> {
    /// Construct buffer info without any registered data.
    pub fn new(primitive: Primitive) -> Self {
        let mut info = MaybeUninit::zeroed();
        let info = unsafe {
            citro3d_sys::BufInfo_Init(info.as_mut_ptr());
            info.assume_init()
        };

        Self {
            info,
            lens: [0; 12],
            indices: None,
            primitive,
            _phantom: PhantomData,
        }
    }

    /// Retrieve a slice
    pub fn buffer(&self, id: u8) -> Option<Slice<'_>> {
        if id as i32 >= self.info.bufCount {
            return None;
        }

        Some(Slice { id, buf_info: self })
    }

    /// Register vertex buffer object data. The lifetime of the passed-in VBO will be tied to the
    /// lifetime of this [`Info`]. `vbo_data` is assumed to use one `T` per drawn primitive, and
    /// its layout is assumed to match the given `attrib_info`.
    ///
    /// # Errors
    ///
    /// Registering VBO data may fail:
    ///
    /// * if `vbo_data` is not allocated with the [`ctru::linear`] allocator
    /// * if the maximum number (12) of VBOs are already registered
    /// * if the length of the vbo_data is too long
    #[doc(alias = "BufInfo_Add")]
    pub fn add<T>(&mut self, vbo_data: &'buf [T], attrib_info: &attrib::Info) -> crate::Result<u8> {
        let stride = std::mem::size_of::<T>().try_into()?;
        let len: i32 = vbo_data
            .len()
            .try_into()
            .map_err(|_| crate::Error::InvalidSize)?;

        // SAFETY: the lifetime of the VBO data is encapsulated in the return value's
        // 'vbo lifetime, and the pointer to &mut self.0 is used to access values
        // in the BufInfo, not copied to be used later.
        let res = unsafe {
            citro3d_sys::BufInfo_Add(
                &mut self.info,
                vbo_data.as_ptr().cast(),
                stride,
                attrib_info.attr_count(),
                attrib_info.permutation(),
            )
        };

        // Error codes from <https://github.com/devkitPro/citro3d/blob/master/source/buffers.c#L11>
        let index = match res {
            ..=-3 => Err(crate::Error::System(res)),
            -2 => Err(crate::Error::InvalidMemoryLocation),
            -1 => Err(crate::Error::TooManyBuffers),
            _ => Ok(res.try_into().expect("Unexpected result from BufInfo_Add")),
        }?;

        self.lens[index as usize] = len;

        Ok(index)
    }

    /// Register index buffer data. The lifetime of the passed-in buffer will be tied to the
    /// lifetime of this [`Info`]. `indices` must contain an array of either [`u8`] or [`u16`] elements.
    ///
    /// # Errors
    ///
    /// Registering VBO data may fail:
    ///
    /// * if `indices` is not allocated with the [`ctru::linear`] allocator
    /// * if the length of the index array is too long
    pub fn add_indices<I: Index>(&mut self, indices: &'buf [I]) -> crate::Result<()> {
        let ptr = indices.as_ptr();
        if !crate::is_linear_ptr(ptr) {
            return Err(crate::Error::InvalidMemoryLocation);
        }

        let len = indices
            .len()
            .try_into()
            .map_err(|_| crate::Error::InvalidSize)?;

        self.indices = Some(Indices {
            pointer: ptr.cast(),
            len,
            index_type: I::TYPE,
        });

        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct Indices {
    pub(crate) pointer: *const libc::c_void,
    pub(crate) len: i32,
    pub(crate) index_type: i32,
}

/// A slice of buffer data. This borrows the buffer data and can be thought of
/// as similar to `&[T]` obtained by slicing a `Vec<T>`.
#[derive(Debug, Clone, Copy)]
pub struct Slice<'buf> {
    id: u8,
    buf_info: &'buf Info<'buf>,
}

impl Slice<'_> {
    /// Get the id of the buffer for this slice.
    pub fn id(&self) -> u8 {
        self.id
    }

    /// Get the buffer info this slice is associated with.
    pub fn info(&self) -> &Info {
        self.buf_info
    }
}

pub trait Index: crate::private::Sealed {
    const TYPE: libc::c_int;
}
impl Index for u8 {
    const TYPE: libc::c_int = citro3d_sys::C3D_UNSIGNED_BYTE as _;
}
impl Index for u16 {
    const TYPE: libc::c_int = citro3d_sys::C3D_UNSIGNED_SHORT as _;
}

/// The geometric primitive to draw (i.e. what shapes the buffer data describes).
#[repr(u16)]
#[derive(Debug, Clone, Copy)]
#[doc(alias = "GPU_Primitive_t")]
pub enum Primitive {
    /// Draw triangles (3 vertices per triangle).
    Triangles = ctru_sys::GPU_TRIANGLES,
    /// Draw a triangle strip (each vertex shared by 1-3 triangles).
    TriangleStrip = ctru_sys::GPU_TRIANGLE_STRIP,
    /// Draw a triangle fan (first vertex shared by all triangles).
    TriangleFan = ctru_sys::GPU_TRIANGLE_FAN,
    /// Geometry primitive. Can be used for more complex use cases like geometry
    /// shaders that output custom primitives.
    GeometryPrim = ctru_sys::GPU_GEOMETRY_PRIM,
}
