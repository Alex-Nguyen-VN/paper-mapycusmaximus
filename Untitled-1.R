# Exploring the implementation using Shiny

To support interactive exploration of Focus–Glue–Context (FGC) parameters, the package includes a Shiny-based *lens explorer*. The app allows users to experiment with fisheye settings on a realistic spatial example - Victorian LGAs with a synthetic sampled hospital - Residential Aged Care Facility (RACF) transfer network during COVID 19 - and to observe how points, lines, and polygons respond under a shared geometric warp.

## App structure

```{r shiny-screenshot, fig.cap= "Interactive Focus-Glue-Context Shiny application. Users control lens centre, radii, zoom, and compression via sliders and drag the lens directly on the map, with points, lines, and polygons warped together in real time. The app enables rapid exploration of fisheye parameters before committing to static figures.", out.width="80%"}

knitr::include_graphics("figures/shiny_screenshot.jpeg")
```

The interface is divided into two coordinated panels:

-   **Fisheye (drag lens)**: an SVG-based view rendered in the browser. The fisheye center can be repositioned by dragging directly on the map, triggering real-time geometric warping without re-running server-side plotting code.
-   **Original (static)**: a conventional `ggplot2`/`sf` map rendered with the *same bounding box and aspect ratio* as the fisheye view, enabling fair visual comparison between warped and unwarped representations.

Spatial data (LGAs, points, and transfer lines) are converted once into plain coordinate lists and sent to the browser. Subsequent interactions update only lens parameters, ensuring smooth response even during continuous dragging.

## Interactive workflow

### Selecting the lens center

Users begin by choosing an **Initial lens center (LGA)** from the sidebar. Internally, the selected LGA polygon is reduced to a representative point (`st_point_on_surface()`), which becomes the fisheye center. This mirrors the common scripted workflow of passing a polygon or centroid as the `center` argument to `sf_fisheye()`.

Once initialized, the center can be moved freely by dragging within the fisheye panel, allowing rapid scanning of different regions without changing parameters.

### Adjusting focus and glue radii

Two sliders control the spatial extent of distortion:

-   **Inner radius (focus)** -\> `r_in` Sets the size of the magnified region. Smaller values create a tight focal bubble; larger values expand the magnified area.
-   **Outer radius (glue)** -\> `r_out` Sets the extent of the transition zone where compression occurs before geometry becomes fixed.

Together, these define the Focus–Glue–Context structure used by both `fisheye_fgc()` and `sf_fisheye()`. Increasing `r_out` spreads distortion more gradually; decreasing it concentrates deformation closer to the focus.

### Controlling magnification and compression

Two further sliders adjust distortion strength:

-   **Zoom factor** -\> `zoom_factor` / `zoom` Controls radial expansion inside the focus. Higher values increase separation of dense features but amplify distortion near the focus boundary.
-   **Squeeze** -\> `squeeze_factor` / `squeeze` Controls how sharply distances compress within the glue region. Smaller values produce a pronounced “shoulder” near boundaries; larger values yield a smoother transition.

Users are encouraged to increase zoom until local structure becomes readable, then adjust squeeze to reduce crowding or sharp curvature near the glue boundary.

### Sampling and layer visibility

The network is intentionally subsampled to maintain interpretability:

-   **Sample size per layer** (`n_fac`) controls how many hospitals and RACFs are included.
-   **Resample facilities** draws a new random subset to test robustness of visual conclusions.
-   **Show transfer lines** toggles line geometry on and off, allowing users to tune parameters using points alone before validating connectivity.

All layers (polygons, points, and lines) are warped together using identical parameters, ensuring alignment is preserved under distortion.

## What to look for

When using the app, readers should pay attention to:

-   Whether dense metropolitan features separate cleanly in the focus.
-   Whether transfer lines remain connected to their endpoints under distortion.
-   How stable the surrounding context remains as the lens moves or parameters change.

These observations help users choose sensible parameter ranges before producing static figures or scripted analyses.

## Design rationale

The app deliberately separates *parameter exploration* from *final figure generation*. Interactive dragging and sliders provide immediate visual feedback, while the parameter values correspond directly to arguments in `sf_fisheye()`. Once suitable settings are identified, the same values can be reused verbatim in reproducible code pipelines.

## Future work

For the current approach, the shiny app only use the default dataset included in the `mapycusmaximus` package, which are the 2016 Victorian Local Government Areas (LGA) and their boundaries, accompany with the synthetic transfer network between hospital and RACF. In the future stable, we will open up the app for users to upload their own spatial data or piping it directly to the Shiny app from R.

# Application to Victorian ambulance transfer records

We focus on one application: Victorian hospitals, residential aged care facilities (RACFs) and ambluance transfer act as connection between these two during COVID-19 period. All of the data was synthetic, except for the location of the hospitals and RACF, which is publicly available. We sample 10 hospitals and 10 RACFs to avoid clutter, use a grey basemap for contrast, and highlight how points, polygons, and connection lines behave under the same lens. 