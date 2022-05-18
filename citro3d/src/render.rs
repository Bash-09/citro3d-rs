//! This module provides render target types and options for controlling transfer
//! of data to the GPU, including the format of color and depth data to be rendered.

use citro3d_sys::{
    C3D_RenderTarget, C3D_RenderTargetCreate, C3D_RenderTargetDelete, C3D_DEPTHTYPE, GPU_COLORBUF,
    GPU_DEPTHBUF,
};
use ctru::gfx;
use ctru::services::gspgpu::FramebufferFormat;

use crate::{Error, Result};

mod transfer;

/// A render target for `citro3d`. Frame data will be written to this target
/// to be rendered on the GPU and displayed on the screen.
pub struct Target {
    raw: *mut citro3d_sys::C3D_RenderTarget,
    color_format: ColorFormat,
}

impl Drop for Target {
    fn drop(&mut self) {
        unsafe {
            C3D_RenderTargetDelete(self.raw);
        }
    }
}

impl Target {
    /// Create a new render target with the specified size, color format,
    /// and depth format.
    ///
    /// # Errors
    ///
    /// Fails if the specified sizes are invalid, or the target could not be
    /// created.
    pub fn new(
        width: u32,
        height: u32,
        color_format: ColorFormat,
        depth_format: DepthFormat,
    ) -> Result<Self> {
        let raw = unsafe {
            C3D_RenderTargetCreate(
                width.try_into()?,
                height.try_into()?,
                color_format as GPU_COLORBUF,
                depth_format.as_raw(),
            )
        };

        if raw.is_null() {
            Err(Error::FailedToInitialize)
        } else {
            Ok(Self { raw, color_format })
        }
    }

    /// Sets the screen to actually display the output of this render target.
    pub fn set_output(&mut self, screen: &impl gfx::Screen, side: gfx::Side) {
        let framebuf_format = screen.get_framebuffer_format();

        let flags = transfer::Flags::default()
            .in_format(self.color_format.into())
            .out_format(ColorFormat::from(framebuf_format).into());

        unsafe {
            citro3d_sys::C3D_RenderTargetSetOutput(
                self.raw,
                screen.as_raw(),
                side.into(),
                flags.bits(),
            );
        }
    }

    /// Clear the render target with the given 32-bit RGBA color and depth buffer value.
    /// Use `flags` to specify whether color and/or depth should be overwritten.
    pub fn clear(&mut self, flags: ClearFlags, rgba_color: u32, depth: u32) {
        unsafe {
            citro3d_sys::C3D_RenderTargetClear(self.raw, flags.bits(), rgba_color, depth);
        }
    }

    /// Return the underlying `citro3d` render target for this target.
    pub(crate) fn as_raw(&self) -> *mut C3D_RenderTarget {
        self.raw
    }
}

bitflags::bitflags! {
    /// Indicate whether color, depth buffer, or both values should be cleared.
    pub struct ClearFlags: u32 {
        const COLOR = citro3d_sys::C3D_CLEAR_COLOR;
        const DEPTH = citro3d_sys::C3D_CLEAR_DEPTH;
        const ALL = citro3d_sys::C3D_CLEAR_ALL;
    }
}

/// The color format to use when rendering on the GPU.
#[repr(u32)]
#[derive(Clone, Copy, Debug)]
pub enum ColorFormat {
    /// 8-bit Red + 8-bit Green + 8-bit Blue + 8-bit Alpha.
    RGBA8 = citro3d_sys::GPU_RB_RGBA8,
    /// 8-bit Red + 8-bit Green + 8-bit Blue.
    RGB8 = citro3d_sys::GPU_RB_RGB8,
    /// 5-bit Red + 5-bit Green + 5-bit Blue + 1-bit Alpha.
    RGBA5551 = citro3d_sys::GPU_RB_RGBA5551,
    /// 5-bit Red + 6-bit Green + 5-bit Blue.
    RGB565 = citro3d_sys::GPU_RB_RGB565,
    /// 4-bit Red + 4-bit Green + 4-bit Blue + 4-bit Alpha.
    RGBA4 = citro3d_sys::GPU_RB_RGBA4,
}

impl From<FramebufferFormat> for ColorFormat {
    fn from(format: FramebufferFormat) -> Self {
        match format {
            FramebufferFormat::Rgba8 => Self::RGBA8,
            FramebufferFormat::Rgb565 => Self::RGB565,
            FramebufferFormat::Rgb5A1 => Self::RGBA5551,
            FramebufferFormat::Rgba4 => Self::RGBA4,
            // this one seems unusual, but it appears to work fine:
            FramebufferFormat::Bgr8 => Self::RGB8,
        }
    }
}

/// The depth buffer format to use when rendering.
#[repr(u32)]
#[derive(Clone, Copy, Debug)]
pub enum DepthFormat {
    /// 16-bit depth.
    Depth16 = citro3d_sys::GPU_RB_DEPTH16,
    /// 24-bit depth.
    Depth24 = citro3d_sys::GPU_RB_DEPTH24,
    /// 24-bit depth + 8-bit Stencil.
    Depth24Stencil8 = citro3d_sys::GPU_RB_DEPTH24_STENCIL8,
}

impl DepthFormat {
    fn as_raw(self) -> C3D_DEPTHTYPE {
        C3D_DEPTHTYPE {
            __e: self as GPU_DEPTHBUF,
        }
    }
}