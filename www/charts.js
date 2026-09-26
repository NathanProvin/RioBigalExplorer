// Custom echarts renderers for the infographic charts (wired from R as htmlwidgets::JS("bigal.xxx(...)")).
window.bigal = {
  // Violin drawn as stacked trapezoids: item = [group, y0, y1, halfWidth0, halfWidth1] (half widths in category units)
  violin: function (colors) {
    return function (params, api) {
      var k = api.value(0), bw = api.size([1, 0])[0];
      var a = api.coord([k, api.value(1)]), b = api.coord([k, api.value(2)]);
      var w0 = api.value(3) * bw, w1 = api.value(4) * bw;
      return { type: 'polygon', silent: false,
        shape: { points: [[a[0] - w0, a[1]], [b[0] - w1, b[1]], [b[0] + w1, b[1]], [a[0] + w0, a[1]]] },
        style: { fill: colors[k % colors.length], opacity: 0.38, stroke: colors[k % colors.length], lineWidth: 0.4 } };
    };
  },
  // Grey silhouette behind a group: item = [group, lo, hi, iconIndex].
  // fit = true: fitted inside the band between lo and hi; fit = false: height is exactly lo..hi (size proportional to value).
  silhouette: function (imgs, aspect, fit) {
    return function (params, api) {
      var k = api.value(0), i = api.value(3), ar = aspect[i] || 1;
      var lo = api.coord([k, api.value(1)]), hi = api.coord([k, api.value(2)]);
      var span = Math.abs(lo[1] - hi[1]), top = Math.min(lo[1], hi[1]), bw = api.size([1, 0])[0], w, hh;
      if (fit) { var h = Math.max(24, span); w = Math.min(bw * 0.95, h * ar); hh = w / ar; top = top + (span - hh) / 2; }
      else { hh = span; w = span * ar; }
      return { type: 'image', silent: true,
        style: { image: imgs[i], x: lo[0] - w / 2, y: top, width: w, height: hh, opacity: fit ? 0.13 : 0.16 } };
    };
  },
  // Line segment in data coordinates (dendrograms): item = [x1, y1, x2, y2]
  segment: function (color) {
    return function (params, api) {
      var a = api.coord([api.value(0), api.value(1)]), b = api.coord([api.value(2), api.value(3)]);
      return { type: 'line', silent: true, shape: { x1: a[0], y1: a[1], x2: b[0], y2: b[1] }, style: { stroke: color, lineWidth: 1.5 } };
    };
  },
  // Pictogram: repeated icons, one per `unit`, the last one clipped to the fraction.
  // item = [category, value, unit, background]; cfg = {icon, aspect, horizontal, offset (band fraction), size, bg}
  pictogram: function (cfg) {
    return function (params, api) {
      var cat = api.value(0), v = api.value(1), unit = api.value(2), bgv = api.value(3), ch = [];
      var H = cfg.horizontal, band = H ? api.size([0, 1])[1] : api.size([1, 0])[0];
      var base = H ? api.coord([0, cat]) : api.coord([cat, 0]);
      var cell = H ? api.size([unit, 0])[0] : api.size([0, unit])[1];
      var s = Math.min(cfg.size || 22, band * 0.72, cell * 0.95), w = s * (cfg.aspect || 1);
      var off = (cfg.offset || 0) * band;
      if (bgv > 0) {
        var L = H ? api.size([bgv, 0])[0] : api.size([0, bgv])[1];
        ch.push(H ? { type: 'rect', shape: { x: base[0], y: base[1] - s * 0.62 + off, width: L, height: s * 1.24, r: s * 0.62 }, style: { fill: cfg.bg || '#EFE6CF' } }
                  : { type: 'rect', shape: { x: base[0] - w * 0.62 + off, y: base[1] - L, width: w * 1.24, height: L, r: w * 0.62 }, style: { fill: cfg.bg || '#EFE6CF' } });
      }
      var n = Math.ceil(v / unit - 1e-9);
      for (var i = 0; i < n; i++) {
        var frac = Math.min(1, v / unit - i);
        var cx = H ? base[0] + (i + 0.5) * cell : base[0] + off, cy = H ? base[1] + off : base[1] - (i + 0.5) * cell;
        var img = { type: 'image', style: { image: cfg.icon, x: cx - w / 2, y: cy - s / 2, width: w, height: s } };
        if (frac < 1) img.clipPath = H ? { type: 'rect', shape: { x: cx - w / 2, y: cy - s / 2, width: w * frac, height: s } }
                                      : { type: 'rect', shape: { x: cx - w / 2, y: cy + s / 2 - s * frac, width: w, height: s * frac } };
        ch.push(img);
      }
      return { type: 'group', children: ch };
    };
  },
  // Circle pack in a unit box: item = [x, y, r, kind, index]; kind 0 = bubble (colour index), 1 = cluster outline, 2 = cluster badge (icon + label)
  pack: function (cfg) {
    return function (params, api) {
      var W = api.getWidth(), H = api.getHeight(), s = Math.min(W, H - 20) / 2 * 0.92, cx = W / 2, cy = (H - 20) / 2 + 8;
      var x = cx + api.value(0) * s, y = cy + api.value(1) * s, r = api.value(2) * s, kind = api.value(3), i = api.value(4);
      if (kind === 0) return { type: 'circle', shape: { cx: x, cy: y, r: Math.max(r, 1.5) },
        style: { fill: cfg.colors[i], stroke: '#FBF8F0', lineWidth: 1.2 } };
      if (kind === 1) return { type: 'circle', silent: true, shape: { cx: x, cy: y, r: r },
        style: { fill: 'rgba(30,77,43,0.045)', stroke: '#A9BCA3', lineWidth: 1, lineDash: [4, 3] } };
      var b = Math.max(11, Math.min(18, r * 0.3));
      return { type: 'group', children: [
        { type: 'circle', shape: { cx: x, cy: y - r, r: b }, style: { fill: '#FBF8F0', stroke: '#8FA88A', lineWidth: 1 } },
        { type: 'image', style: { image: cfg.icons[i], x: x - b * 0.62, y: y - r - b * 0.62, width: b * 1.24, height: b * 1.24 } },
        { type: 'text', style: { text: cfg.labels[i], x: x, y: y - r + b + 2, align: 'center', verticalAlign: 'top',
          fill: '#1F2A22', font: '600 11px Inter, sans-serif', backgroundColor: 'rgba(251,248,240,0.9)', padding: [1, 5], borderRadius: 4 } }
      ] };
    };
  }
};
