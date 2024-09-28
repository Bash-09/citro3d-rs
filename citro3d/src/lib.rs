#![feature(allocator_api)]
#![feature(custom_test_frameworks)]
#![test_runner(test_runner::run_gdb)]
#![feature(doc_cfg)]
#![feature(doc_auto_cfg)]
#![doc(html_root_url = "https://rust3ds.github.io/citro3d-rs/crates")]
#![doc(
    html_favicon_url = "https://user-images.githubusercontent.com/11131775/225929072-2fa1741c-93ae-4b47-9bdf-af70f3d59910.png"
)]
#![doc(
    html_logo_url = "https://user-images.githubusercontent.com/11131775/225929072-2fa1741c-93ae-4b47-9bdf-af70f3d59910.png"
)]

//! Safe Rust bindings to `citro3d`. This crate wraps `citro3d-sys` to provide
//! safer APIs for graphics programs targeting the 3DS.
//!
//! ## Feature flags
#![doc = document_features::document_features!()]

pub mod attrib;
pub mod buffer;
pub mod error;
pub mod math;
pub mod render;
pub mod shader;
pub mod texenv;
pub mod texture;
pub mod uniform;

use std::cell::{OnceCell, RefMut};
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

use ctru::services::gfx::Screen;
pub use error::{Error, Result};

use self::buffer::{Index, Indices};
use self::texenv::TexEnvInner;
use self::uniform::Uniform;

pub mod macros {
    //! Helper macros for working with shaders.
    pub use citro3d_macros::*;
}

mod private {
    pub trait Sealed {}
    impl Sealed for u8 {}
    impl Sealed for u16 {}
}

/// The single instance for using `citro3d`. This is the base type that an application
/// should instantiate to use this library.
#[non_exhaustive]
#[must_use]
pub struct Instance {
    texenvs: [OnceCell<TexEnvInner>; texenv::TEXENV_COUNT],
    queue: Rc<RenderQueue>,
}

/// Representation of `citro3d`'s internal render queue. This is something that
/// lives in the global context, but it keeps references to resources that are
/// used for rendering, so it's useful for us to have something to represent its
/// lifetime.
struct RenderQueue;

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instance").finish_non_exhaustive()
    }
}

impl Instance {
    /// Initialize the default `citro3d` instance.
    ///
    /// # Errors
    ///
    /// Fails if `citro3d` cannot be initialized.
    pub fn new() -> Result<Self> {
        Self::with_cmdbuf_size(citro3d_sys::C3D_DEFAULT_CMDBUF_SIZE.try_into().unwrap())
    }

    /// Initialize the instance with a specified command buffer size.
    ///
    /// # Errors
    ///
    /// Fails if `citro3d` cannot be initialized.
    #[doc(alias = "C3D_Init")]
    pub fn with_cmdbuf_size(size: usize) -> Result<Self> {
        if unsafe { citro3d_sys::C3D_Init(size) } {
            Ok(Self {
                texenvs: [
                    // thank goodness there's only six of them!
                    OnceCell::new(),
                    OnceCell::new(),
                    OnceCell::new(),
                    OnceCell::new(),
                    OnceCell::new(),
                    OnceCell::new(),
                ],
                queue: Rc::new(RenderQueue),
            })
        } else {
            Err(Error::FailedToInitialize)
        }
    }

    /// Create a new render target with the specified size, color format,
    /// and depth format.
    ///
    /// # Errors
    ///
    /// Fails if the target could not be created with the given parameters.
    #[doc(alias = "C3D_RenderTargetCreate")]
    #[doc(alias = "C3D_RenderTargetSetOutput")]
    pub fn render_target<'screen>(
        &self,
        width: usize,
        height: usize,
        screen: RefMut<'screen, dyn Screen>,
        depth_format: Option<render::DepthFormat>,
    ) -> Result<render::Target<'screen>> {
        render::Target::new(width, height, screen, depth_format, Rc::clone(&self.queue))
    }

    /// Select the given render target for drawing the frame. This must be called
    /// as pare of a render call (i.e. within the call to
    /// [`render_frame_with`](Self::render_frame_with)).
    ///
    /// # Errors
    ///
    /// Fails if the given target cannot be used for drawing, or called outside
    /// the context of a frame render.
    ///
    /// SAFETY: The target must live until it's no longer in use
    #[doc(alias = "C3D_FrameDrawOn")]
    unsafe fn select_render_target(&mut self, target: &render::Target<'_>) -> Result<()> {
        let _ = self;
        if unsafe { citro3d_sys::C3D_FrameDrawOn(target.as_raw()) } {
            Ok(())
        } else {
            Err(Error::InvalidRenderTarget)
        }
    }

    pub fn begin_new_frame(&mut self) -> Frame {
        Frame::new(self)
    }

    /// Render a frame. The passed in function/closure can mutate the instance,
    /// such as to [select a render target](Self::select_render_target)
    /// or [bind a new shader program](Self::bind_program).
    #[doc(alias = "C3D_FrameBegin")]
    #[doc(alias = "C3D_FrameEnd")]
    pub fn render_frame_with<'s>(&'s mut self, f: impl FnOnce(&mut Frame<'_, 's>)) {
        let mut guard = self.begin_new_frame();
        f(&mut guard);
        // not strictly needed but explicit for readability
        drop(guard);
    }

    /// Get the buffer info being used, if it exists. Note that the resulting
    /// [`buffer::Info`] is copied from the one currently in use.
    #[doc(alias = "C3D_GetBufInfo")]
    pub fn buffer_info(&self) -> Option<buffer::Info> {
        let raw = unsafe { citro3d_sys::C3D_GetBufInfo() };
        buffer::Info::copy_from(raw)
    }

    /// Set the buffer info to use for any following draw calls.
    #[doc(alias = "C3D_SetBufInfo")]
    fn set_buffer_info(&mut self, buffer_info: &buffer::Info) {
        let raw: *const _ = &buffer_info.0;
        // SAFETY: C3D_SetBufInfo actually copies the pointee instead of mutating it.
        unsafe { citro3d_sys::C3D_SetBufInfo(raw.cast_mut()) };
    }

    /// Get the attribute info being used, if it exists. Note that the resulting
    /// [`attrib::Info`] is copied from the one currently in use.
    #[doc(alias = "C3D_GetAttrInfo")]
    pub fn attr_info(&self) -> Option<attrib::Info> {
        let raw = unsafe { citro3d_sys::C3D_GetAttrInfo() };
        attrib::Info::copy_from(raw)
    }

    /// Set the attribute info to use for any following draw calls.
    #[doc(alias = "C3D_SetAttrInfo")]
    fn set_attr_info(&mut self, attr_info: &attrib::Info) {
        let raw: *const _ = &attr_info.0;
        // SAFETY: C3D_SetAttrInfo actually copies the pointee instead of mutating it.
        unsafe { citro3d_sys::C3D_SetAttrInfo(raw.cast_mut()) };
    }

    /// Render primitives from the current vertex array buffer.
    #[doc(alias = "C3D_DrawArrays")]
    fn draw_arrays(&mut self, primitive: buffer::Primitive, vbo_data: buffer::Slice) {
        self.set_buffer_info(vbo_data.info());

        // TODO: should we also require the attrib info directly here?
        unsafe {
            citro3d_sys::C3D_DrawArrays(
                primitive as ctru_sys::GPU_Primitive_t,
                vbo_data.index(),
                vbo_data.len(),
            );
        }
    }

    /// Indexed drawing
    ///
    /// Draws the vertices in `buf` indexed by `indices`. `indices` must be linearly allocated
    ///
    /// # Safety
    // TODO: #41 might be able to solve this:
    /// If `indices` goes out of scope before the current frame ends it will cause a
    /// use-after-free (possibly by the GPU).
    ///
    /// # Panics
    ///
    /// If the given index buffer is too long to have its length converted to `i32`.
    #[doc(alias = "C3D_DrawElements")]
    unsafe fn draw_elements<I: Index>(
        &mut self,
        primitive: buffer::Primitive,
        vbo_data: buffer::Slice,
        indices: &Indices<I>,
    ) {
        self.set_buffer_info(vbo_data.info());

        let indices = &indices.buffer;
        let elements = indices.as_ptr().cast();

        citro3d_sys::C3D_DrawElements(
            primitive as ctru_sys::GPU_Primitive_t,
            indices.len().try_into().unwrap(),
            // flag bit for short or byte
            I::TYPE,
            elements,
        );
    }

    /// Use the given [`shader::Program`] for subsequent draw calls.
    ///
    /// # Safety
    /// - `program` must live as long as it's in use (until no more rendering occurs, or
    /// another program is bound to replace it) or UB.
    /// - The memory location pointed to by the reference must not change after this call or UB (i.e. `program` must be pinned)
    unsafe fn bind_program(&mut self, program: &shader::Program) {
        // This will copy the pointer to the internal context, which will result in
        // accessing undefined memory if it moves (e.g. because it went out of scope)
        citro3d_sys::C3D_BindProgram(program.as_raw().cast_mut());
    }

    /// Bind a uniform to the given `index` in the vertex shader for the next draw call.
    ///
    /// # Example
    ///
    /// ```
    /// # let _runner = test_runner::GdbRunner::default();
    /// # use citro3d::uniform;
    /// # use citro3d::math::Matrix4;
    /// #
    /// # let mut instance = citro3d::Instance::new().unwrap();
    /// let idx = uniform::Index::from(0);
    /// let mtx = Matrix4::identity();
    /// instance.bind_vertex_uniform(idx, &mtx);
    /// ```
    pub fn bind_vertex_uniform(&mut self, index: uniform::Index, uniform: impl Into<Uniform>) {
        uniform.into().bind(self, shader::Type::Vertex, index);
    }

    /// Bind a uniform to the given `index` in the geometry shader for the next draw call.
    ///
    /// # Example
    ///
    /// ```
    /// # let _runner = test_runner::GdbRunner::default();
    /// # use citro3d::uniform;
    /// # use citro3d::math::Matrix4;
    /// #
    /// # let mut instance = citro3d::Instance::new().unwrap();
    /// let idx = uniform::Index::from(0);
    /// let mtx = Matrix4::identity();
    /// instance.bind_geometry_uniform(idx, &mtx);
    /// ```
    pub fn bind_geometry_uniform(&mut self, index: uniform::Index, uniform: impl Into<Uniform>) {
        uniform.into().bind(self, shader::Type::Geometry, index);
    }

    /// Retrieve the [`TexEnv`] for the given stage, initializing it first if necessary.
    ///
    /// # Example
    ///
    /// ```
    /// # use citro3d::texenv;
    /// # let _runner = test_runner::GdbRunner::default();
    /// # let mut instance = citro3d::Instance::new().unwrap();
    /// let stage0 = texenv::Stage::new(0).unwrap();
    /// let texenv0 = instance.texenv(stage0);
    /// ```
    #[doc(alias = "C3D_GetTexEnv")]
    #[doc(alias = "C3D_TexEnvInit")]
    fn texenv(&mut self, stage: texenv::Stage) -> &mut texenv::TexEnvInner {
        let texenv = &mut self.texenvs[stage.0];
        texenv.get_or_init(|| TexEnvInner::new(stage));
        // We have to do this weird unwrap to get a mutable reference,
        // since there is no `get_mut_or_init` or equivalent
        texenv.get_mut().unwrap()
    }
}

// This only exists to be an alias, which admittedly is kinda silly. The default
// impl should be equivalent though, since RenderQueue has a drop impl too.
impl Drop for Instance {
    #[doc(alias = "C3D_Fini")]
    fn drop(&mut self) {}
}

impl Drop for RenderQueue {
    fn drop(&mut self) {
        unsafe {
            citro3d_sys::C3D_Fini();
        }
    }
}

pub struct Frame<'i, 'r> {
    instance: &'i mut Instance,
    _render_pass_phantom: PhantomData<&'r ()>,
}

impl<'i, 'r> Frame<'i, 'r> {
    fn new(inst: &'i mut Instance) -> Frame<'i, 'r> {
        unsafe {
            citro3d_sys::C3D_FrameBegin(
                // TODO: begin + end flags should be configurable
                citro3d_sys::C3D_FRAME_SYNCDRAW.try_into().unwrap(),
            );
        }
        Self {
            instance: inst,
            _render_pass_phantom: PhantomData,
        }
    }

    #[doc(alias = "C3D_DrawArrays")]
    #[doc(alias = "C3D_DrawElements")]
    pub fn draw<I: Index>(&mut self, pass: RenderPass<'r, '_, '_, '_, I>) -> Result<()> {
        let RenderPass {
            program,
            target,
            vbo_data,
            attribute_info,
            texenv_stages,
            params:
                RenderParameters {
                    primitive,
                    indices,
                    vertex_uniforms,
                    geometry_uniforms,
                },
        } = pass;

        if texenv_stages.is_empty() {
            return Err(Error::InvalidSize);
        }

        unsafe {
            self.instance.bind_program(program);
            self.instance.select_render_target(target).unwrap();

            // Uniforms
            self.instance.set_attr_info(attribute_info);

            for &(index, uniform) in vertex_uniforms.iter() {
                self.instance.bind_vertex_uniform(index, uniform);
            }

            for &(index, uniform) in geometry_uniforms.iter() {
                self.instance.bind_geometry_uniform(index, uniform);
            }

            // Texenvs
            for i in 0..texenv::TEXENV_COUNT {
                let texenv = self.instance.texenv(texenv::Stage::new(i).unwrap());
                if let Some(stage) = texenv_stages.get(i) {
                    stage.setup_texenv(texenv);
                } else {
                    texenv.reset();
                }
            }

            // Draw arrays or elements
            if let Some(indices) = indices {
                self.instance.draw_elements(primitive, vbo_data, indices);
            } else {
                self.instance.draw_arrays(primitive, vbo_data);
            }
        }

        // Ok(self)
        Ok(())
    }
}
impl Drop for Frame<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            citro3d_sys::C3D_FrameEnd(0);
        }
    }
}

/// A RenderPass describes all the parameters for making a call to render a vbo.
#[derive(Clone)]
pub struct RenderPass<'k, 'buf, 'tgt, 'arr, I: Index> {
    pub program: &'k shader::Program,
    pub target: &'k render::Target<'tgt>,
    pub vbo_data: buffer::Slice<'buf>,
    pub attribute_info: &'k attrib::Info,
    /// The [`texenv::TexEnv`] stages used to combine shader outputs and textures.
    /// There must be at least 1 and at most 6 provided, any more than 6 will be ignored.
    pub texenv_stages: &'arr [texenv::TexEnv<'k>],
    pub params: RenderParameters<'arr, 'buf, I>,
}

#[derive(Clone)]
pub struct RenderParameters<'arr, 'buf, I: Index> {
    pub primitive: buffer::Primitive,
    pub indices: Option<&'arr Indices<'buf, I>>,
    pub vertex_uniforms: &'arr [(uniform::Index, Uniform)],
    pub geometry_uniforms: &'arr [(uniform::Index, Uniform)],
}

impl<I: Index> Default for RenderParameters<'_, '_, I> {
    fn default() -> Self {
        Self {
            primitive: buffer::Primitive::Triangles,
            indices: None,
            vertex_uniforms: &[],
            geometry_uniforms: &[],
        }
    }
}

#[cfg(test)]
mod tests {
    use ctru::services::gfx::Gfx;

    use super::*;

    #[test]
    fn select_render_target() {
        let gfx = Gfx::new().unwrap();
        let screen = gfx.top_screen.borrow_mut();

        let mut instance = Instance::new().unwrap();
        let target = instance.render_target(10, 10, screen, None).unwrap();

        instance.render_frame_with(|instance| {
            instance.select_render_target(&target).unwrap();
        });

        // Check that we don't get a double-free or use-after-free by dropping
        // the global instance before dropping the target.
        drop(instance);
        drop(target);
    }
}
