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


Shiny.addCustomMessageHandler('input-knob-attr', function(message) {
  const id = message.id;
  const param = message.attr;
  const val = message.value;
  document.getElementById(id)[[param]] = val;
});

Shiny.addCustomMessageHandler('input-knob-call', function(message) {
  const id = message.id;
  const method = message.method;
  const args = message.args;
  document.getElementById(id)[[method]](args);
});

Shiny.addCustomMessageHandler('input-knob-get', function(message) {
  const id = message.id;
  const param = message.attr;
  const cbid = message.cbid;
  const val = document.getElementById(id)[[param]];
  Shiny.setInputValue(cbid, val, { priority: "event" });
});
