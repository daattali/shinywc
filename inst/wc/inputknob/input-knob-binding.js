// Send web component events to Shiny
$(document).on('knob-move-change', 'input-knob', function(evt) {
  const id = evt.target.id + '_knob-move-change';
  let val = evt.detail;
  if (typeof(val) === "undefined") {
    val = true;
  }
  Shiny.setInputValue(id, val, { priority: "event" });
});

$(document).on('knob-move-start', 'input-knob', function(evt) {
  const id = evt.target.id + '_knob-move-start';
  let val = evt.detail;
  if (typeof(val) === "undefined") {
    val = true;
  }
  Shiny.setInputValue(id, val, { priority: "event" });
});

$(document).on('knob-move-end', 'input-knob', function(evt) {
  const id = evt.target.id + '_knob-move-end';
  let val = evt.detail;
  if (typeof(val) === "undefined") {
    val = true;
  }
  Shiny.setInputValue(id, val, { priority: "event" });
});


Shiny.addCustomMessageHandler('input-knob-attr-set', function(message) {
  const id = message.id;
  const param = message.attr;
  const val = message.value;
  document.getElementById(id)[[param]] = val;
});

Shiny.addCustomMessageHandler('input-knob-attr-get', function(message) {
  const id = message.id;
  const param = message.attr;
  const cbid = message.cbid;
  const val = document.getElementById(id)[[param]];
  Shiny.setInputValue(cbid, val, { priority: "event" });
});

Shiny.addCustomMessageHandler('input-knob-call', function(message) {
  const id = message.id;
  const method = message.method;
  const args = message.args;
  document.getElementById(id)[[method]](args);
});


const shinywcMutationObserver = new MutationObserver(function(mutations) {
  if (mutations.length == 1 && mutations[0].type === 'attributes' && mutations[0].target.id !== '') {
    const id = mutations[0].target.id;
    const attr = mutations[0].attributeName;
    const val = document.getElementById(id)[[attr]];
    const cbid = id + '_knob-attr-change';
    Shiny.setInputValue(cbid, { [attr] : val }, { priority: "event" });
  }
});
Shiny.addCustomMessageHandler('input-knob-init', function(message) {
  const id = message.id;
  const el = document.getElementById(id);
  setTimeout(function() {
    shinywcMutationObserver.observe(el, { attributes: true, subtree: false, childList: false });
  }, 0);

  const attributes = message.attributes;
  const cbid = id + '_knob-attr-change';
  attributes.forEach(function (attr, idx) {
    Shiny.setInputValue(cbid, { [attr] : el[[attr]] }, { priority: "event" });
  });
});
