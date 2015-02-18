
Array.prototype.flatMap = function(lambda) {
    return Array.prototype.concat.apply([], this.map(lambda));
};

$(document).ready(function() {
  var loadTime = window.performance.timing.responseEnd-window.performance.timing.navigationStart;
  var x = $("#pagetime")
  //(loadTime / 1000) + "." + ((loadTime % 1000) / 100)
  x.html(loadTime + "ms")
})

function goto() {
  function fields(param, areaID) {
    var fieldsArray = $(areaID).sortable("toArray")
    return (fieldsArray.length == 0) ? "" : "&" + param + "=" + fieldsArray.join(":")
  }
  window.location.href = "?a=1" +
      fields("filters", "#filterarea") +
      fields("rows", "#rowarea") +
      fields("columns", "#columnarea") +
      fields("measures", "#measurearea") +
      "&" + $('#pivotform').serialize()
}

function initReport() {
  $(function() {
    var marketDay = $("#marketday");
    marketDay.datepicker({dateFormat: 'yy-mm-dd'});
    function changeDay(amount) {
        var date=new Date(marketDay.datepicker('getDate'));
        date.setDate(date.getDate()+amount);
        marketDay.datepicker('setDate', date);
    }
    $("#prevday").click(function() { changeDay(-1); goto(); return false; });
    $("#nextday").click(function() { changeDay(1); goto(); return false; });
    $("#pivotform").change(goto)
  });
}

function initPivot() {
  var change = function( event, ui ) {
    if (this === ui.item.parent()[0]) { goto() }
  }
  $(".area").sortable({
    connectWith: ".area,#backgroundlist",
    placeholder: "placeholder",
    forcePlaceholderSize: false,
    update: change,
    revert: 200,
    start: function (event, ui) {
      //Modifying the placeholder for two reasons:
      // 1) We want it to have the dragged item's text
      // 2) If we don't set the html to something (even "" will do) it gets extra padding. No idea why.
      ui.placeholder.html(ui.item.html())
      ui.placeholder.css({ "border-color": "#00FFFF" })
    }
  })
  $("#backgroundlist").sortable({
    update: change,
  })
}


function connect(onMessage) {
  var socket = $.atmosphere;
  var subSocket;

  var request = {
    url: "/live",
    contentType: "application/json",
    logLevel: 'debug',
    transport: 'websocket',
    fallbackTransport: 'long-polling',
    reconnectInterval: 5000
  };

  request.onOpen = function(response) {
    console.log("Open")
    // Seems there is a scalatra bug which prevents sending messages on connect so we send an empty message
    // from the client after connecting which the server then responds to with the initial report
    subSocket.push( JSON.stringify({ action: "init" }))
  };

  request.onReconnect = function(rq, rs) {
    console.log("reconnect")
  };

  request.onMessage = onMessage

  request.onClose = function(rs) {
    console.log("close")
  };

  request.onError = function(rs) {
    console.log("error")
  };

  subSocket = socket.subscribe(request);
  return subSocket
}

function startEvcel() {
  var x = ((typeof global === "object" && global && global["Object"] === Object) ? global : this)
  var y= x["evcel"]["webjs"]["JSMain"]()
  y.main()
}


function initMithril() {

  var evcel = {}; //mithril module?

  evcel.response = m.prop({
    "riskReportParameters": {
      "marketDay": { "day": "x", "timeOfDay": {"pricesCanMove": false, "fixingsShouldExist": true} },
      "environmentParams": {
         "showEqFutures": false,
         "baseCCY": "USD",
         "valuationCcy": "USD",
         "tenor": ""
      }
    },
    "pivot": {
      "spares": [], "rows": [], "filters": [], "columns": [], "measures":[],
      "rowsWidth":1, "measuresWidth":1, "table": []
    }
  })

  evcel.pivot = function() { return evcel.response().pivot }
  evcel.riskReportParameters = function() { return evcel.response().riskReportParameters }


  //the view-model tracks a running list of todos,
  //stores a description for new todos before they are created
  //and takes care of the logic surrounding when adding is permitted
  //and clearing the input after adding a todo to the list
  evcel.vm = (function() {
    var vm = {}
    vm.init = function() {
      vm.pending = {};
      vm.moved = new Set()
      vm.rightOf = {}
    }
    vm.updateParam = function(name,value) {
      vm.pending[name]=value
      vm.changePage()
    }
    vm.changePage = function() {
      evcel.send( { action:"page", "arg": {
        "risk":evcel.riskReportParameters(),
        "override": vm.pending,
        "rightOf": vm.rightOf
      } } )
    }
    vm.fieldsfilter = m.prop("")
    vm.spares = function() {
      var text = evcel.vm.fieldsfilter()
      if (text === "") {
        return evcel.response().pivot.spares
      } else {
        return evcel.response().pivot.spares.filter(function(field) { return field.toLowerCase().indexOf(text) > -1 })
      }
    }

    vm.moveToRightOf = function(dragged, toArea, toRightOfField) {
      vm.moved.add(dragged)
      var existing = vm.rightOf[ [toArea,toRightOfField] ]
      if (existing) {
        vm.rightOf[ [toArea,dragged] ] = existing
      }
      vm.rightOf[ [toArea,toRightOfField] ] = dragged
      vm.changePage()
    }
    var preview = function(area, fields) {

      var addRightOfs = function(field) {
        var rightOf = vm.rightOf[ [area,field] ]
        var i = 0
        while (rightOf && i < 100) {
          result.push(rightOf)
          rightOf = vm.rightOf[ [area,rightOf] ]
          i = i + 1
        }
      }

      var result = []

      var first = vm.rightOf[ [area,undefined] ]
      if (first) {
        result.push(first)
        console.log(vm.rightOf)
        addRightOfs(first)
      }

      fields.map(function (field) {
        if (! vm.moved.has(field)) {
          result.push(field)
          addRightOfs(field)
        }
      })

      return result
    }
    vm.rows = function() { return preview("rowarea", evcel.response().pivot.rows) }
    vm.filters = function() { return preview("filterarea", evcel.response().pivot.filters) }
    vm.columns = function() { return preview("columnarea", evcel.response().pivot.columns) }
    vm.measures = function() { return preview("measurearea", evcel.response().pivot.measures) }
    vm.measuresWidth = function() { return evcel.response().pivot.measuresWidth }
    vm.rowsWidth = function() { return evcel.response().pivot.rowsWidth }
    vm.table = function() { return evcel.response().pivot.table }
    return vm
  }())

  //the controller defines what part of the model is relevant for the current page
  //in our case, there's only one view-model that handles everything
  evcel.controller = function() {
    evcel.vm.init()
  }

  evcel.form = {}
  evcel.view = function() { return evcel.pivot(evcel.reportParameters()) }

  function binds(data) {
    return {onchange: function(e) {
      //data[e.target.name] = e.target.value;
      //console.log("Fired" + e.target.name)
    }};
  };

  function datePicker(ctrl, name, text, extract) {
    var value = ctrl.pending[name] || extract(evcel.riskReportParameters())
    var special = function(element, isInitialized) {
      if (!isInitialized) {
        $("#marketday").datepicker({dateFormat: 'yy-mm-dd'});
      }
    }
//        function changeDay(amount) {
//            var date=new Date(marketDay.datepicker('getDate'));
//            date.setDate(date.getDate()+amount);
//            marketDay.datepicker('setDate', date);
//        }
//        $("#prevday").click(function() { changeDay(-1); goto(); return false; });
//        $("#nextday").click(function() { changeDay(1); goto(); return false; });
    return m("label", {"for": name}, [
      m("input", { id: name, name: name, type:"text", size: 10, value: value,
        onchange: function() { ctrl.updateParam(name, this.value)}, config: special}),
      m("button.arrow", {}, "<"),
      m("button.arrow", {}, ">")
    ])
  }

  function checkbox(ctrl, name, text, extract) {
   var value = ctrl.pending[name] || extract(evcel.riskReportParameters())
   return m("label", {"for": name}, [
     m("input", {
       "id": name, "name": name, "type":"checkbox",
       "checked": value, onchange: function() { ctrl.updateParam(name, this.checked)} }),
     text
   ])
  }

  function selectCCY(ctrl, name, text, extract) {
   var value = ctrl.pending[name] || extract(evcel.riskReportParameters())
   return m("label", {"for": name}, [
     m("select", {
       "id": name, "name": name,
       "checked": value, onchange: function() { ctrl.updateParam(name, this.value)} },
       [ "USD", "GBP", "EUR"].map(function(ccy) { return m("option", {value:ccy, selected:ccy==value}, ccy)})
     ), text
   ])
  }

  evcel.reportParameters = function() {
    return m("form#pivotform", binds(evcel.form), [
      m("fieldset", [
        datePicker(evcel.vm, "marketday", "Market Day", function(param) { return param.marketDay.day }),
        checkbox(evcel.vm, "pricescanmove", "Prices Can Move", function(param) { return param.marketDay.timeOfDay.pricesCanMove }),
        checkbox(evcel.vm, "fixingsshouldexist", "Fixings Should Exist", function (param) { return param.marketDay.timeOfDay.fixingsShouldExist }),
        checkbox(evcel.vm, "zeroir", "Zero IR", function (param) { return param.zeroIR }),
        checkbox(evcel.vm, "showeqfutures", "Show Eq Futures", function (param) { return param.environmentParams.showEqFutures }),
        selectCCY(evcel.vm, "baseccy", "Base CCY", function (param) { return param.environmentParams.baseCCY}),
        selectCCY(evcel.vm, "valuationccy", "Valuation CCY", function (param) { return param.environmentParams.valuationCcy})
      ])
    ])
  }

  evcel.drag = (function() {
    var d = {}
    d.dragged = undefined
    d.area = undefined
    d.field = undefined
    d.dragging = function(dragged) { d.dragged = dragged }
    d.mark = function (area,field) { d.area = area; d.field = field}
    d.marked = function(fieldarea, field) { return (d.area == fieldarea && d.field == field) }
    d.end = function() {
      evcel.vm.moveToRightOf(d.dragged, d.area, d.field)
      d.area = undefined; d.field = undefined;
    }
    return d
  })()

  evcel.pivot = function(parameters) {
    var fieldArea = function(fieldareaid, fields) {
      var createMarker = function(on) { return m("div.marker", {class: on ? "marked" : ""}) }
      var createFieldLI = function (field,index) {
        return [ m("li", {
          id: field,
          key:field,
          'data-id': field,
          draggable: 'true',
          ondragend: function(e) { evcel.drag.end() },
          ondragstart: function(e) {
            e.dataTransfer.effectAllowed = 'move'
            e.dataTransfer.setData('text/html', null)
            evcel.drag.dragging(e.currentTarget.dataset.id)
          }
        }, field), createMarker(evcel.drag.marked(fieldareaid, field)) ]
      }
      var dragover = function(e) {
        e.preventDefault()
        var ul = e.currentTarget
        var rightOf = undefined
        $(ul).children('li').each(function (index,li) {
          var jqLi = $(li)
          var liMiddle = jqLi.offset().left + (jqLi.width() / 2)
          if (e.clientX > liMiddle) {
            rightOf = jqLi
          }
        })
        if (rightOf) {
          evcel.drag.mark(fieldareaid, rightOf.attr("id"))
        } else {
          evcel.drag.mark(fieldareaid, undefined)
        }
      }
      var clear = function(e) {
        evcel.drag.mark(undefined, undefined)
      }
      var firstMarker = createMarker(evcel.drag.marked(fieldareaid, undefined))
      return m(
        "ul.area", { id:fieldareaid, ondragover: dragover, ondragleave: clear },
        [firstMarker].concat(fields.flatMap(createFieldLI))
      )
    }
    return m("div", [
      parameters,
      "Filter", m("input", {oninput: m.withAttr("value", evcel.vm.fieldsfilter), value: evcel.vm.fieldsfilter()}),
      fieldArea("fieldarea", evcel.vm.spares()),
      m("div.mainTable", [
        fieldArea("filterarea", evcel.vm.filters()),
        m("table", [
          m("tr", [
            m("td", {"colspan":evcel.vm.rowsWidth()}),
            m("td.fieldareacell", {"colspan": evcel.vm.measuresWidth()}, [
              fieldArea("columnarea", evcel.vm.columns())
            ])
          ]),
          m("tr", [
            m("td.fieldareacell#rowareacell", { "colspan": evcel.vm.rowsWidth()}, [
              fieldArea("rowarea", evcel.vm.rows())
            ]),
            m("td.fieldareacell", {"colspan": evcel.vm.measuresWidth()}, [
              fieldArea("measurearea", evcel.vm.measures())
            ])
          ]) ].concat(
            evcel.vm.table().map(function(row,index) {
              return m("tr", row.map(function (cell, index) {
                return m(cell.isHeading ? "th" : "td", {"colspan":cell.width, "rowspan": cell.height }, cell.name);
              }))
            })
          )
        )
      ])
    ])
  };
//        m("input", {onchange: m.withAttr("value", todo.vm.description), value: todo.vm.description()}),
//        m("button", {onclick: todo.vm.add}, "Add"),
//        m("table", [
//            todo.vm.list.map(function(task, index) {
//                return m("tr", [
//                    m("td", [
//                        m("input[type=checkbox]", {onclick: m.withAttr("checked", task.done), checked: task.done()})
//                    ]),
//                    m("td", {style: {textDecoration: task.done() ? "line-through" : "none"}}, task.description()),
//                ])
//            })
//        ])
//    ])

  //initialize the application
  var main = document.getElementById("body")
  m.module(main, {controller: evcel.controller, view: evcel.view});

  function onMessage(rs) {
    m.startComputation();
    var message = rs.responseBody;
    try {
      var json = jQuery.parseJSON(message);
      console.log("got a message")
      evcel.response(json)

    } catch (e) {
      console.log('This doesn\'t look like a valid JSON object: ', message.data);
      return;
    }
    m.endComputation();
  }
  var socket = connect(onMessage)
  evcel.send = function(msg) { console.log("SEND: " + msg); socket.push(JSON.stringify(msg)) }


}