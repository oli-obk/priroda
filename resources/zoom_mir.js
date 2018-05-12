function enable_mir_mousewheel() {
  var svg = svgPanZoom("#mir > svg", {
    controlIconsEnabled: true,
    zoomScaleSensitivity: 0.5,
    maxZoom: 50.0,
    minZoom: 1.0,
    fit: true,
  });
}
