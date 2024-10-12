//! Texture combiner support. See <https://www.khronos.org/opengl/wiki/Texture_Combiners>
//! for more details.

use std::fmt::Debug;

use bitflags::bitflags;

use crate::texture;

// https://oreo639.github.io/citro3d/texenv_8h.html#a9eda91f8e7252c91f873b1d43e3728b6
pub(crate) const TEXENV_COUNT: usize = 6;

#[doc(alias = "C3D_TexEnv")]
pub(crate) struct TexEnvInner(*mut citro3d_sys::C3D_TexEnv);

impl TexEnvInner {
    pub(crate) fn new(stage: Stage) -> Self {
        let mut result = unsafe { Self(citro3d_sys::C3D_GetTexEnv(stage.0 as _)) };
        result.reset();
        result
    }

    /// Re-initialize the texture combiner to its default state.
    pub(crate) fn reset(&mut self) {
        unsafe {
            citro3d_sys::C3D_TexEnvInit(self.0);
        }
    }

    /// Mark this texenv as dirty to ensure it is updated on the GPU
    /// before any following draw calls.
    pub(crate) fn dirty(&mut self) {
        unsafe {
            citro3d_sys::C3D_DirtyTexEnv(self.0);
        }
    }
}

/// Configure the texture combination function.
#[derive(Debug, Clone, Copy)]
pub struct Func {
    /// The [`Mode`]\(s) the combination function will apply to.
    pub mode: Mode,
    /// The [`CombineFunc`] used to combine textures.
    pub combine: CombineFunc,
}

impl Func {
    /// Set the combine func for the given TexEnv
    #[doc(alias = "C3D_TexEnvFunc")]
    unsafe fn set_func(&self, env: &mut TexEnvInner) {
        citro3d_sys::C3D_TexEnvFunc(env.0, self.mode.bits(), self.combine as _);
    }
}

/// Configure the source values of the texture combiner.
#[derive(Clone, Copy)]
pub struct Sources<'a> {
    /// Which [`Mode`]\(s) to set the sourc operand(s) for.
    pub mode: Mode,
    /// The first [`Source`] operand to the texture combiner
    pub source0: Source<'a>,
    /// Optional additional [`Source`] operand to use
    pub source1: Option<Source<'a>>,
    /// Optional additional [`Source`] operand to use
    pub source2: Option<Source<'a>>,
}

impl Sources<'_> {
    /// Set the sources for the given TexEnv, and binds the textures to the appropriate texture units.
    #[doc(alias = "C3D_TexEnvSrc")]
    #[doc(alias = "C3D_TexBind")]
    unsafe fn set_and_bind_texture_sources(&self, env: &mut TexEnvInner) {
        let handle_source = |source| {
            match source {
                Source::Texture0(t) => t.bind(texture::TexUnit::TexUnit0),
                Source::Texture1(t) => t.bind(texture::TexUnit::TexUnit1),
                Source::Texture2(t) => t.bind(texture::TexUnit::TexUnit2),
                Source::Texture3(t) => t.bind(texture::TexUnit::TexUnit3),
                _ => {}
            };
        };

        let source0 = self.source0;
        let source1 = self.source1.unwrap_or(Source::PrimaryColor);
        let source2 = self.source2.unwrap_or(Source::PrimaryColor);

        handle_source(source0);
        handle_source(source1);
        handle_source(source2);

        citro3d_sys::C3D_TexEnvSrc(
            env.0,
            self.mode.bits(),
            SourceInner::from(source0) as _,
            SourceInner::from(source1) as _,
            SourceInner::from(source2) as _,
        );
    }
}

/// Configure the operations on the texture combiner
#[derive(Clone, Copy)]
pub struct Ops {
    pub rgb0: RGBOp,
    pub rgb1: RGBOp,
    pub rgb2: RGBOp,
    pub alpha0: AlphaOp,
    pub alpha1: AlphaOp,
    pub alpha2: AlphaOp,
}

impl Ops {
    pub const fn default() -> Ops {
        Ops {
            rgb0: RGBOp::SrcColor,
            rgb1: RGBOp::SrcColor,
            rgb2: RGBOp::SrcColor,
            alpha0: AlphaOp::SrcAlpha,
            alpha1: AlphaOp::SrcAlpha,
            alpha2: AlphaOp::SrcAlpha,
        }
    }
}

impl Default for Ops {
    fn default() -> Self {
        Ops::default()
    }
}

impl Ops {
    fn set_ops(&self, env: &TexEnvInner) {
        unsafe {
            citro3d_sys::C3D_TexEnvOpRgb(env.0, self.rgb0 as _, self.rgb1 as _, self.rgb2 as _);
            citro3d_sys::C3D_TexEnvOpAlpha(
                env.0,
                self.alpha0 as _,
                self.alpha1 as _,
                self.alpha2 as _,
            );
        }
    }
}

/// A texture combiner, also called a "texture environment" (hence the struct name).
/// See also [`texenv.h` documentation](https://oreo639.github.io/citro3d/texenv_8h.html).
#[derive(Clone, Copy)]
#[doc(alias = "C3D_TexEnv")]
pub struct TexEnv<'a> {
    pub func: Func,
    pub sources: Sources<'a>,
    pub ops: Ops,
}

pub const DEFAULT_TEXENV: TexEnv<'static> = TexEnv {
    func: Func {
        mode: Mode::BOTH,
        combine: CombineFunc::Replace,
    },
    sources: Sources {
        mode: Mode::BOTH,
        source0: Source::PrimaryColor,
        source1: None,
        source2: None,
    },
    ops: Ops::default(),
};

impl TexEnv<'_> {
    pub(crate) unsafe fn setup_texenv(&self, env: &mut TexEnvInner) {
        self.sources.set_and_bind_texture_sources(env);
        self.func.set_func(env);
        self.ops.set_ops(env);
    }
}

bitflags! {
    /// Whether to operate on colors, alpha values, or both.
    #[doc(alias = "C3D_TexEnvMode")]
    pub struct Mode: citro3d_sys::C3D_TexEnvMode {
        #[allow(missing_docs)]
        const RGB = citro3d_sys::C3D_RGB;
        #[allow(missing_docs)]
        const ALPHA = citro3d_sys::C3D_Alpha;
        #[allow(missing_docs)]
        const BOTH = citro3d_sys::C3D_Both;
    }
}

/// A source operand of a [`TexEnv`]'s texture combination.
#[doc(alias = "GPU_TEVSRC")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[non_exhaustive]
pub(crate) enum SourceInner {
    PrimaryColor = ctru_sys::GPU_PRIMARY_COLOR,
    FragmentPrimaryColor = ctru_sys::GPU_FRAGMENT_PRIMARY_COLOR,
    FragmentSecondaryColor = ctru_sys::GPU_FRAGMENT_SECONDARY_COLOR,
    Texture0 = ctru_sys::GPU_TEXTURE0,
    Texture1 = ctru_sys::GPU_TEXTURE1,
    Texture2 = ctru_sys::GPU_TEXTURE2,
    Texture3 = ctru_sys::GPU_TEXTURE3,
    PreviousBuffer = ctru_sys::GPU_PREVIOUS_BUFFER,
    Constant = ctru_sys::GPU_CONSTANT,
    Previous = ctru_sys::GPU_PREVIOUS,
}

/// A source operand of a [`TexEnv`]'s texture combination, containing the textures to be used.
#[doc(alias = "GPU_TEVSRC")]
#[derive(Clone, Copy)]
pub enum Source<'a> {
    PrimaryColor,
    FragmentPrimaryColor,
    FragmentSecondaryColor,
    Texture0(&'a texture::Texture),
    Texture1(&'a texture::Texture),
    Texture2(&'a texture::Texture),
    Texture3(&'a texture::Texture),
    PreviousBuffer,
    Constant,
    Previous,
}

impl Debug for Source<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", SourceInner::from(self))
    }
}

impl From<&Source<'_>> for SourceInner {
    fn from(value: &Source<'_>) -> Self {
        match value {
            Source::PrimaryColor => SourceInner::PrimaryColor,
            Source::FragmentPrimaryColor => SourceInner::FragmentPrimaryColor,
            Source::FragmentSecondaryColor => SourceInner::FragmentSecondaryColor,
            Source::Texture0(_) => SourceInner::Texture0,
            Source::Texture1(_) => SourceInner::Texture1,
            Source::Texture2(_) => SourceInner::Texture2,
            Source::Texture3(_) => SourceInner::Texture3,
            Source::PreviousBuffer => SourceInner::PreviousBuffer,
            Source::Constant => SourceInner::Constant,
            Source::Previous => SourceInner::Previous,
        }
    }
}

impl From<Source<'_>> for SourceInner {
    fn from(value: Source<'_>) -> Self {
        SourceInner::from(&value)
    }
}

/// The combination function to apply to the [`TexEnv`] operands.
#[doc(alias = "GPU_COMBINEFUNC")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[non_exhaustive]
pub enum CombineFunc {
    Replace = ctru_sys::GPU_REPLACE,
    Modulate = ctru_sys::GPU_MODULATE,
    Add = ctru_sys::GPU_ADD,
    AddSigned = ctru_sys::GPU_ADD_SIGNED,
    Interpolate = ctru_sys::GPU_INTERPOLATE,
    Subtract = ctru_sys::GPU_SUBTRACT,
    Dot3Rgb = ctru_sys::GPU_DOT3_RGB,
    // Added in libcrtu 2.3.0:
    // Dot3Rgba = ctru_sys::GPU_DOT3_RGBA,
}

/// The RGB combiner operands.
#[doc(alias = "GPU_TEVOP_RGB")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[non_exhaustive]
pub enum RGBOp {
    SrcColor = ctru_sys::GPU_TEVOP_RGB_SRC_COLOR,
    OneMinusSrcColor = ctru_sys::GPU_TEVOP_RGB_ONE_MINUS_SRC_COLOR,
    SrcAlpha = ctru_sys::GPU_TEVOP_RGB_SRC_ALPHA,
    OneMinusSrcAlpha = ctru_sys::GPU_TEVOP_RGB_ONE_MINUS_SRC_ALPHA,
    SrcRed = ctru_sys::GPU_TEVOP_RGB_SRC_R,
    OneMinusSrcRed = ctru_sys::GPU_TEVOP_RGB_ONE_MINUS_SRC_R,
    SrcGreen = ctru_sys::GPU_TEVOP_RGB_SRC_G,
    OneMinusSrcGreen = ctru_sys::GPU_TEVOP_RGB_ONE_MINUS_SRC_G,
    SrcBlue = ctru_sys::GPU_TEVOP_RGB_SRC_B,
    OneMinusSrcBlue = ctru_sys::GPU_TEVOP_RGB_ONE_MINUS_SRC_B,
}

/// The Alpha combiner operands.
#[doc(alias = "GPU_TEVOP_RGB")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
#[non_exhaustive]
pub enum AlphaOp {
    SrcAlpha = ctru_sys::GPU_TEVOP_A_SRC_ALPHA,
    OneMinusSrcAlpha = ctru_sys::GPU_TEVOP_A_ONE_MINUS_SRC_ALPHA,
    SrcRed = ctru_sys::GPU_TEVOP_A_SRC_R,
    OneMinusSrcRed = ctru_sys::GPU_TEVOP_A_ONE_MINUS_SRC_R,
    SrcGreen = ctru_sys::GPU_TEVOP_A_SRC_G,
    OneMinusSrcGreen = ctru_sys::GPU_TEVOP_A_ONE_MINUS_SRC_G,
    SrcBlue = ctru_sys::GPU_TEVOP_A_SRC_B,
    OneMinusSrcBlue = ctru_sys::GPU_TEVOP_A_ONE_MINUS_SRC_B,
}

/// A texture combination stage identifier. This index doubles as the order
/// in which texture combinations will be applied.
// (I think?)
#[derive(Copy, Clone, Debug)]
pub(crate) struct Stage(pub(crate) usize);

impl Stage {
    /// Get a stage index. Valid indices range from 0 to 5.
    pub(crate) fn new(index: usize) -> Option<Self> {
        (index < 6).then_some(Self(index))
    }
}
