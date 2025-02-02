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
pub mod light;
pub mod material;
pub mod math;
pub mod render;
pub mod shader;
pub mod texenv;
pub mod texture;
pub mod uniform;

use std::cell::RefMut;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

use ctru::services::gfx::Screen;
pub use error::{Error, Result};
use texenv::TEXENV_COUNT;

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
    ) -> Result<render::ScreenTarget<'screen>> {
        render::ScreenTarget::new(width, height, screen, depth_format, Rc::clone(&self.queue))
    }

    /// Create a new render target that renders to a texture with the specified size, color format,
    /// and depth format.
    ///
    /// # Errors
    ///
    /// Fails if the target could not be created with the given parameters.
    pub fn render_target_texture(
        &self,
        texture: texture::Texture,
        face: texture::Face,
        depth_format: Option<render::DepthFormat>,
    ) -> Result<render::TextureTarget> {
        render::TextureTarget::new(texture, face, depth_format, Rc::clone(&self.queue))
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
    unsafe fn select_render_target<T: render::Target>(&mut self, target: &T) -> Result<()> {
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

    /// Set the buffer info to use for any following draw calls.
    #[doc(alias = "C3D_SetBufInfo")]
    fn set_buffer_info(&mut self, buffer_info: &buffer::Info) {
        let raw: *const _ = &buffer_info.info;
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

    /// Render primitives from the current vertex array buffer, with indices if indices have been set in the [`buffer::Info`].
    #[doc(alias = "C3D_DrawArrays")]
    #[doc(alias = "C3D_DrawElements")]
    fn draw(&mut self, vbo_data: buffer::Slice) {
        let info = vbo_data.info();

        self.set_buffer_info(info);

        if let Some(inds) = &info.indices {
            unsafe {
                citro3d_sys::C3D_DrawElements(
                    info.primitive as ctru_sys::GPU_Primitive_t,
                    inds.len,
                    // flag bit for short or byte
                    inds.index_type,
                    inds.pointer,
                );
            }
        } else {
            unsafe {
                citro3d_sys::C3D_DrawArrays(
                    info.primitive as ctru_sys::GPU_Primitive_t,
                    vbo_data.id() as _,
                    info.lens[vbo_data.id() as usize] as i32,
                );
            }
        }
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
    pub fn bind_vertex_uniform<'a>(
        &mut self,
        index: uniform::Index,
        uniform: impl Into<Uniform<'a>>,
    ) {
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
    pub fn bind_geometry_uniform<'a>(
        &mut self,
        index: uniform::Index,
        uniform: impl Into<Uniform<'a>>,
    ) {
        uniform.into().bind(self, shader::Type::Geometry, index);
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
    pub fn draw<T: render::Target>(&mut self, pass: &RenderPass<'r, '_, '_, T>) -> Result<()> {
        let RenderPass {
            program,
            target,
            vbo_data,
            attribute_info,
            texenv_stages,
            textures,
            vertex_uniforms,
            geometry_uniforms,
            lightenv,
        } = pass;

        // Ensure all required textures are present
        for src in texenv_stages
            .iter()
            .take(TEXENV_COUNT)
            .flat_map(|te| te.sources.iter())
        {
            let texunit = match src {
                texenv::Source::Texture0 => Some(texture::TexUnit::TexUnit0),
                texenv::Source::Texture1 => Some(texture::TexUnit::TexUnit1),
                texenv::Source::Texture2 => Some(texture::TexUnit::TexUnit2),
                texenv::Source::Texture3 => Some(texture::TexUnit::TexUnit3),
                _ => None,
            };

            if let Some(texunit) = texunit {
                if textures[texunit as usize].is_none() {
                    return Err(Error::MissingTexture(texunit));
                }
            }
        }

        unsafe {
            self.instance.bind_program(program);
            self.instance.select_render_target(*target).unwrap();

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
                if let Some(texenv) = texenv_stages.get(i) {
                    texenv
                        .set_texenv(i)
                        .expect("Texenv stage should always be in bounds");
                } else {
                    let texenv = citro3d_sys::C3D_GetTexEnv(i as i32);
                    if !texenv.is_null() {
                        citro3d_sys::C3D_TexEnvInit(texenv);
                        citro3d_sys::C3D_DirtyTexEnv(texenv);
                    }
                }
            }

            // Textures
            for (texture, texunit) in textures.iter().zip(texture::TEXUNITS) {
                if let Some(texture) = texture {
                    texture.bind(texunit);
                }
            }

            // Light env
            let lightenv: *mut citro3d_sys::C3D_LightEnv = lightenv
                .map(|le| le.as_raw() as *const _ as *mut _)
                .unwrap_or(core::ptr::null_mut());
            citro3d_sys::C3D_LightEnvBind(lightenv);

            // Draw arrays or elements
            self.instance.draw(*vbo_data);
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
/// `'l` has a long life and must be alive until the end of the frame
/// `'s` has a short life and must only be alive as long as this render pass is
#[derive(Clone)]
pub struct RenderPass<'l, 's, 'buf, T: render::Target> {
    pub program: &'l shader::Program,
    pub target: &'l T,
    pub vbo_data: buffer::Slice<'buf>,
    pub attribute_info: &'l attrib::Info,
    /// The [`texenv::TexEnv`] stages used to combine shader outputs and textures.
    /// There must be at least 1 and at most 6 provided, any more than 6 will be ignored.
    pub texenv_stages: Vec<texenv::TexEnv>,
    pub textures: [Option<&'l texture::Texture>; 4],
    pub vertex_uniforms: Vec<(uniform::Index, Uniform<'s>)>,
    pub geometry_uniforms: Vec<(uniform::Index, Uniform<'s>)>,
    pub lightenv: Option<&'l light::LightEnv>,
}

impl<'l, 's, 'buf, T: render::Target> RenderPass<'l, 's, 'buf, T> {
    pub fn new(
        program: &'l shader::Program,
        target: &'l T,
        vbo_data: buffer::Slice<'buf>,
        attribute_info: &'l attrib::Info,
    ) -> RenderPass<'l, 's, 'buf, T> {
        RenderPass {
            program,
            target,
            vbo_data,
            attribute_info,
            texenv_stages: Vec::new(),
            textures: [None; 4],
            vertex_uniforms: Vec::new(),
            geometry_uniforms: Vec::new(),
            lightenv: None,
        }
    }
}

impl<'l, 's, 'buf, 'arr, T: render::Target> RenderPass<'l, 's, 'buf, T> {
    pub fn with_program(mut self, program: &'l shader::Program) -> RenderPass<'l, 's, 'buf, T> {
        self.program = program;
        self
    }

    pub fn with_target<T2: render::Target>(self, target: &'l T2) -> RenderPass<'l, 's, 'buf, T2> {
        RenderPass {
            program: self.program,
            target,
            vbo_data: self.vbo_data,
            attribute_info: self.attribute_info,
            texenv_stages: self.texenv_stages,
            textures: self.textures,
            vertex_uniforms: self.vertex_uniforms,
            geometry_uniforms: self.geometry_uniforms,
            lightenv: self.lightenv,
        }
    }

    pub fn with_vbo(
        mut self,
        vbo_data: buffer::Slice<'buf>,
        attribute_info: &'l attrib::Info,
    ) -> RenderPass<'l, 's, 'buf, T> {
        self.vbo_data = vbo_data;
        self.attribute_info = attribute_info;
        self
    }

    pub fn with_texenv_stages<'a>(
        mut self,
        texenvs: impl IntoIterator<Item = &'a texenv::TexEnv>,
    ) -> RenderPass<'l, 's, 'buf, T> {
        self.texenv_stages = texenvs.into_iter().cloned().collect();
        self
    }

    /// Bind the given texture to the chosen texture unit for this render pass
    /// (for use in the [`TexEnv`] stages)
    pub fn with_texture(
        mut self,
        texture_unit: texture::TexUnit,
        texture: &'l texture::Texture,
    ) -> RenderPass<'l, 's, 'buf, T> {
        self.textures[texture_unit as usize] = Some(texture);
        self
    }

    /// Set the vertex uniform values to use for this invocation of the shader program.
    /// This will clear any previously set uniforms and only use the new ones provided.
    pub fn with_vertex_uniforms(
        mut self,
        uniforms: impl IntoIterator<Item = (uniform::Index, Uniform<'s>)>,
    ) -> RenderPass<'l, 's, 'buf, T> {
        self.vertex_uniforms = uniforms.into_iter().collect();
        self
    }

    /// Set the geometry uniform values to use for this invocation of the shader program.
    /// This will clear any previously set uniforms and only use the new ones provided.
    pub fn with_geometry_uniforms(
        mut self,
        uniforms: impl IntoIterator<Item = (uniform::Index, Uniform<'s>)>,
    ) -> RenderPass<'l, 's, 'buf, T> {
        self.geometry_uniforms = uniforms.into_iter().collect();
        self
    }

    pub fn with_lightenv(mut self, lightenv: &'l light::LightEnv) -> RenderPass<'l, 's, 'buf, T> {
        self.lightenv = Some(lightenv);
        self
    }

    pub fn no_lightenv(mut self) -> RenderPass<'l, 's, 'buf, T> {
        self.lightenv = None;
        self
    }
}

/// Check if pointer is in linear memory
pub fn is_linear_ptr<P>(p: *const P) -> bool {
    let addr = p as usize;
    addr >= ctru_sys::OS_FCRAM_VADDR as usize
        && addr < (ctru_sys::OS_FCRAM_VADDR as usize + ctru_sys::OS_FCRAM_SIZE as usize)
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
