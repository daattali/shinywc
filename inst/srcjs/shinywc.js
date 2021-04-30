shinywc = function() {

  // Mapping between each component type and the attributes it supports
  var _componentAttributes = {};

  // Mapping between each component instance and its type
  var _componentTypes = {};

  // List of IDs of components that the server has requested to create an object
  // for, but they haven't yet been initialized on the client side
  var _idsToInit = new Set();

  // Mutation observer to automatically send any changed attributes back to
  // the shiny object representing a component
  var _mutationObserver = new MutationObserver(function(mutations) {
    if (mutations.length == 1 && mutations[0].type === 'attributes' && mutations[0].target.id !== '') {
      const id = mutations[0].target.id;
      const component = _componentTypes[id];
      if (typeof component === "undefined") {
        return;
      }
      const attr = mutations[0].attributeName;
      const val = document.getElementById(id).getAttribute(attr);
      const cbid = `${id}_${component}-attr-change`;
      Shiny.setInputValue(cbid, { [attr] : val }, { priority: "event" });
    }
  });

  var _setupAttributes = function(component, attributes) {
    _componentAttributes[component] = attributes
  };

  var _setupEvents = function(component, events) {
    events.forEach(function(event) { _setupEvent(component, event) });
  };

  var _setupEvent = function(component, eventName) {
    $(document).on(eventName, component, function(evt) {
      const id = `${evt.target.id}_event_${eventName}`;
      let val = evt.detail;
      if (typeof(val) === "undefined") {
        val = true;
      }
      Shiny.setInputValue(id, val, { priority: "event" });
    });
  };

  var _registerComponent = function(component, id) {
    console.log('reg')
    _componentTypes[id] = component;
  };

  var _initComponent = function(id) {
    const component = _componentTypes[id];
    if (typeof component === "undefined") {
      console.log(`shinywc: cannot find component type for ID "${id}"`);
      return;
    }

    const el = document.getElementById(id);
    const attributes = _componentAttributes[component];
    if (typeof attributes === "undefined" || attributes.length == 0) {
      return;
    }

    _mutationObserver.observe(el, { attributes: true, subtree: false, childList: false });
    const cbid = `${id}_${component}-attr-change`;
    attributes.forEach(function (attr, idx) {
      Shiny.setInputValue(cbid, { [attr] : el.getAttribute(attr) }, { priority: "event" });
    });
  };

  return {

    // Set up a component with shinywc. This has to be done once per component
    // type. If you have multiple instances of the same component on the page,
    // this only gets called once.
    setupComponent : function(data) {
      _setupAttributes(data.name, data.attributes);
      _setupEvents(data.name, data.events);
    },

    // Register an instance of a component with its component type
    registerComponent : function(component, id) {
      console.log('ggg');
      _registerComponent(component, id);
      if (_idsToInit.has(id)) {
        _idsToInit.delete(id);
        _initComponent(id);
      }
    },

    initComponent : function(id) {
      const el = document.getElementById(id);
      if (el === null) {
        _idsToInit.add(id);
      } else {
        _initComponent(id);
      }
    }

  };
}();

Shiny.addCustomMessageHandler('shinywc-attr-set', function(message) {
  const id = message.id;
  const param = message.attr;
  const val = message.value;
  document.getElementById(id).setAttribute(param, val);
});

Shiny.addCustomMessageHandler('shinywc-prop-set', function(message) {
  const id = message.id;
  const param = message.prop;
  const val = message.value;
  document.getElementById(id)[[param]] = val;
});

Shiny.addCustomMessageHandler('shinywc-prop-get', function(message) {
  const id = message.id;
  const param = message.prop;
  const cbid = message.cbid;
  const val = document.getElementById(id)[[param]];
  Shiny.setInputValue(cbid, val, { priority: "event" });
});

Shiny.addCustomMessageHandler('shinywc-call-method', function(message) {
  const id = message.id;
  const method = message.method;
  const params = message.params;
  document.getElementById(id)[[method]](...params);
});

Shiny.addCustomMessageHandler('shinywc-init-component', function(message) {
  const id = message.id;
  shinywc.initComponent(id);
});
