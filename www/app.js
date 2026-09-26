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
