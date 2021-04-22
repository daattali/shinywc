//TODO a lot of the code here is generic to any web component and can be extracted out to a common js

// Send web component events to Shiny
$(document).on('dimChanged', 'epiviz-genes-track', function(evt) {
  const id = evt.target.id + '_evt_dimChanged';
  let val = evt.detail;
  if (typeof(val) === "undefined") {
    val = true;
  }
  Shiny.setInputValue(id, val, { priority: "event" });
});

$(document).on('hover', 'epiviz-genes-track', function(evt) {
  const id = evt.target.id + '_evt_hover';
  let val = evt.detail;
  if (typeof(val) === "undefined") {
    val = true;
  }
  Shiny.setInputValue(id, val, { priority: "event" });
});

$(document).on('unHover', 'epiviz-genes-track', function(evt) {
  const id = evt.target.id + '_evt_unHover';
  let val = evt.detail;
  if (typeof(val) === "undefined") {
    val = true;
  }
  Shiny.setInputValue(id, val, { priority: "event" });
});


Shiny.addCustomMessageHandler('epiviz-genes-track-attr-set', function(message) {
  const id = message.id;
  const param = message.attr;
  const val = message.value;
  document.getElementById(id).setAttribute(param, val);
});

Shiny.addCustomMessageHandler('epiviz-genes-track-attr-get', function(message) {
  const id = message.id;
  const param = message.attr;
  const cbid = message.cbid;
  const val = document.getElementById(id).getAttribute(param);
  Shiny.setInputValue(cbid, val, { priority: "event" });
});

Shiny.addCustomMessageHandler('epiviz-genes-track-prop-set', function(message) {
  const id = message.id;
  const param = message.prop;
  const val = message.value;
  document.getElementById(id)[[param]] = val;
});

Shiny.addCustomMessageHandler('epiviz-genes-track-prop-get', function(message) {
  const id = message.id;
  const param = message.prop;
  const cbid = message.cbid;
  const val = document.getElementById(id)[[param]];
  Shiny.setInputValue(cbid, val, { priority: "event" });
});

Shiny.addCustomMessageHandler('epiviz-genes-track-call', function(message) {
  const id = message.id;
  const method = message.method;
  const args = message.args;
  document.getElementById(id)[[method]](args);
});

shinywcEpivizGenesTrack = function() {

  return {

    idsToInit : new Set(),

    mutationObserver : new MutationObserver(function(mutations) {
      if (mutations.length == 1 && mutations[0].type === 'attributes' && mutations[0].target.id !== '') {
        const id = mutations[0].target.id;
        const attr = mutations[0].attributeName;
        const val = document.getElementById(id).getAttribute(attr);
        const cbid = id + '_epiviz-genes-track-attr-change';
        Shiny.setInputValue(cbid, { [attr] : val }, { priority: "event" });
      }
    }),

    checkInit : function(id) {
      if (shinywcEpivizGenesTrack.idsToInit.has(id)) {
        shinywcEpivizGenesTrack.idsToInit.delete(id);
        shinywcEpivizGenesTrack.init(id);
      }
    },

    init : function(id) {
      const el = document.getElementById(id);
      shinywcEpivizGenesTrack.mutationObserver.observe(el, { attributes: true, subtree: false, childList: false });

      const attributes = ["json-data", "chart-colors"];
      const cbid = id + '_epiviz-genes-track-attr-change';
      attributes.forEach(function (attr, idx) {
        Shiny.setInputValue(cbid, { [attr] : el.getAttribute(attr) }, { priority: "event" });
      });
    }

  }
}();

Shiny.addCustomMessageHandler('epiviz-genes-track-init', function(message) {
  const id = message.id;
  const el = document.getElementById(id);
  if (el === null) {
    shinywcEpivizGenesTrack.idsToInit.add(id);
  } else {
    shinywcEpivizGenesTrack.init(id);
  }
});
