/// Texture filters.
#[doc(alias = "GPU_TEXTURE_FILTER_PARAM")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Filter {
    Nearest = ctru_sys::GPU_NEAREST,
    Linear = ctru_sys::GPU_LINEAR,
}

/// Texture wrap modes.
#[doc(alias = "GPU_TEXTURE_WRAP_PARAM")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Wrap {
    ClampToEdge = ctru_sys::GPU_CLAMP_TO_EDGE,
    ClampToBorder = ctru_sys::GPU_CLAMP_TO_BORDER,
    Repeat = ctru_sys::GPU_REPEAT,
    MirroredRepeat = ctru_sys::GPU_MIRRORED_REPEAT,
}

/// Texture modes.
#[doc(alias = "GPU_TEXTURE_MODE_PARAM")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Mode {
    Tex2D = ctru_sys::GPU_TEX_2D,
    CubeMap = ctru_sys::GPU_TEX_CUBE_MAP,
    Shadow2D = ctru_sys::GPU_TEX_SHADOW_2D,
    Projection = ctru_sys::GPU_TEX_PROJECTION,
    ShadowCube = ctru_sys::GPU_TEX_SHADOW_CUBE,
    Disabled = ctru_sys::GPU_TEX_DISABLED,
}

impl Mode {
    pub fn is_2d(&self) -> bool {
        match self {
            Mode::Shadow2D | Mode::Projection | Mode::Disabled | Mode::Tex2D => true,
            Mode::CubeMap | Mode::ShadowCube => false,
        }
    }

    pub fn is_cube(&self) -> bool {
        !self.is_2d()
    }
}

/// Texture faces.
#[doc(alias = "GPU_TEXFACE")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum Face {
    #[default]
    PositiveX = ctru_sys::GPU_POSITIVE_X,
    NegativeX = ctru_sys::GPU_NEGATIVE_X,
    PositiveY = ctru_sys::GPU_POSITIVE_Y,
    NegativeY = ctru_sys::GPU_NEGATIVE_Y,
    PositiveZ = ctru_sys::GPU_POSITIVE_Z,
    NegativeZ = ctru_sys::GPU_NEGATIVE_Z,
}

impl Face {
    pub const TEX2D: Face = Face::PositiveX;
}

/// Supported texture units.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum TexUnit {
    TexUnit0 = 0,
    TexUnit1 = 1,
    TexUnit2 = 2,
    TexUnit3 = 3,
}

/// Supported texture formats.
#[doc(alias = "GPU_TEXCOLOR")]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Format {
    RGBA8 = ctru_sys::GPU_RGBA8,
    RGB8 = ctru_sys::GPU_RGB8,
    RGBA5551 = ctru_sys::GPU_RGBA5551,
    RGB565 = ctru_sys::GPU_RGB565,
    RGBA4 = ctru_sys::GPU_RGBA4,
    LA8 = ctru_sys::GPU_LA8,
    HILO8 = ctru_sys::GPU_HILO8,
    L8 = ctru_sys::GPU_L8,
    A8 = ctru_sys::GPU_A8,
    LA4 = ctru_sys::GPU_LA4,
    L4 = ctru_sys::GPU_L4,
    A4 = ctru_sys::GPU_A4,
    ETC1 = ctru_sys::GPU_ETC1,
    ETC1A4 = ctru_sys::GPU_ETC1A4,
}

impl Format {
    pub const fn bits_per_pixel(&self) -> usize {
        match self {
            Self::RGBA8 => 32,
            Self::RGB8 => 24,
            Self::RGBA5551 | Self::RGB565 | Self::RGBA4 | Self::LA8 | Self::HILO8 => 16,
            Self::L8 | Self::A8 | Self::LA4 | Self::ETC1A4 => 8,
            Self::L4 | Self::A4 | Self::ETC1 => 4,
        }
    }
}
