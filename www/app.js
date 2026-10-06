// Language swap for [data-i18n] elements, including ones rendered later by renderUI.
let bigalDict = null;
function bigalApply(root) {
  if (!bigalDict || !root.querySelectorAll) return;
  const els = root.matches && root.matches('[data-i18n]') ? [root] : [];
  els.concat([...root.querySelectorAll('[data-i18n]')]).forEach(el => {
    const v = bigalDict[el.dataset.i18n];
    if (v !== undefined && el.textContent !== v) el.textContent = v;
  });
}
$(document).on('shiny:connected', () => {
  Shiny.addCustomMessageHandler('i18n', d => { bigalDict = d; bigalApply(document); });
  // Map placement mode: crosshair cursor while a camera is being placed
  Shiny.addCustomMessageHandler('placing', on => document.body.classList.toggle('placing', !!on));
  new MutationObserver(ms => ms.forEach(m => m.addedNodes.forEach(n => n.nodeType === 1 && bigalApply(n))))
    .observe(document.body, { childList: true, subtree: true });
});
// Camera drawer toggle (pure client side)
$(document).on('click', '[data-toggle-drawer]', () => document.body.classList.toggle('drawer-open'));
// Widgets initialised inside a hidden tab have zero width: re-measure when a tab is shown or a card goes full screen
$(document).on('shown.bs.tab bslib.card', () => setTimeout(() => window.dispatchEvent(new Event('resize')), 50));
// PDF export: snapshot the visible page (chart rows, or the map with its legend) with html-to-image, lay it out with jsPDF
// html-to-image can't read the cross-origin Google Fonts sheets: fetch them (latin subsets only) with the woff2 files inlined
const toDataURL = b => new Promise(ok => { const r = new FileReader(); r.onload = () => ok(r.result); r.readAsDataURL(b); });
let bigalFonts = null;
const fontCSS = () => bigalFonts ??= Promise.all([...document.querySelectorAll('link[href*="fonts.googleapis.com/css"]')]
  .map(l => fetch(l.href).then(r => r.text()))).then(async sheets => {
    const css = sheets.join('\n').match(/\/\* latin(-ext)? \*\/\s*@font-face\s*{[^}]*}/g)?.join('\n') || '';
    const urls = [...new Set(css.match(/https:[^)'"]+/g) || [])];
    const data = await Promise.all(urls.map(u => fetch(u).then(r => r.blob()).then(toDataURL)));
    return urls.reduce((c, u, i) => c.split(u).join(data[i]), css);
  }).catch(() => '');
$(document).on('click', '#pdf_export', async function () {
  const btn = this; if (btn.classList.contains('busy')) return;
  btn.classList.add('busy');
  try {
    const visible = el => el.offsetHeight > 0;  // hidden or empty (e.g. no load errors) blocks
    const pane = document.querySelector('.bslib-page-main > .tab-content > .tab-pane.active');
    const map = pane.querySelector('.map-wrap');
    const name = (document.querySelector('.navbar .nav-link.active')?.textContent || '').trim();
    const day = new Date().toISOString().slice(0, 10);
    const skip = n => !(n.classList && n.matches('.toolbox, .drawer, .leaflet-control-zoom, .placing-banner, .info, .tooltip, .bslib-full-screen-enter'));
    const opts = { filter: skip, pixelRatio: 2, quality: .92, backgroundColor: '#F6F1E4',
                   fontEmbedCSS: await fontCSS() };
    const pdf = new jspdf.jsPDF({ orientation: map ? 'landscape' : 'portrait', unit: 'mm', format: 'a4' });
    const W = pdf.internal.pageSize.getWidth(), H = pdf.internal.pageSize.getHeight(), M = 10;
    pdf.setFontSize(9); pdf.setTextColor('#1E4D2B');
    pdf.text(`RioBigal Explorer — ${name} — ${day}`, M, M - 3);
    let y = M;
    for (const el of map ? [map] : [...pane.children].filter(visible)) {
      const px = el.offsetHeight + 8;  // slack: cloned text renders a hair taller and clipped the page subtitles
      let w = W - 2 * M, h = w * px / el.offsetWidth;
      if (h > H - 2 * M) { w *= (H - 2 * M) / h; h = H - 2 * M; }  // taller than a page: shrink to fit
      if (y + h > H - M && y > M) { pdf.addPage(); y = M; }        // never split a chart row across pages
      // cream fills the corners; the block keeps its own background (backgroundColor alone would repaint the green species banner)
      const bg = getComputedStyle(el).backgroundColor;
      const own = bg === 'rgba(0, 0, 0, 0)' ? {} : { style: { backgroundColor: bg } };
      pdf.addImage(await htmlToImage.toJpeg(el, { ...opts, ...own, height: px }), 'JPEG', M, y, w, h);
      y += h + 4;
    }
    pdf.save(`RioBigal_${name.replace(/[^\p{L}\p{N}]+/gu, '_')}_${day}.pdf`);
  } catch (e) { console.error('PDF export failed', e); }
  finally { btn.classList.remove('busy'); }
});
