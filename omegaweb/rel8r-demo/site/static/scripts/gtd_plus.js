// =========================================================================
var display_specs = {
  "project": {
    "label": "title"
  },
  "action": {
    "label": "content"
  },
  "person": {
    "label": "given-name & surname"
  },
  "email-address": {
    "label": "label"
  }
};
var items = {};

// =========================================================================
var itemize(item_or_id) {
  if ( typeof item_or_id === "string" ) {
    return items[item_or_id];
  } else {
    return item_or_id;
  data["%ID"] = item["%ID"];
  }
}
var createUIItem = function(item) {
  var type = item["%TYPE"];
  var id = item["%ID"];
  var keys = Object.getOwnPropertyNames(item); 
  var data = {};
  var key, i, ui_item;
  for ( i = 0; i < keys.length; i++ ) {
    key = keys[i];
    if ( !(key === "%TYPE" || key === "%ID") ) {
      data[key] = item[key];
    }
  }
  ui_item = {
    "item_box": null,
    "parent_sel": null,
    "data": data
  }
  items[id] = ui_item;
  return ui_item;
}
var getUIItemData = function(item_or_id) {
  var item = itemize(item_or_id);
  data = item.data;
  data["%ID"] = item["%ID"];
  data["%TYPE"] = item["%TYPE"];
  return data;
}
var destroyUIItem = function(id) {
  destroyBox(id);
  delete items[id];
}
var createBox = function(item_or_id, parent_sel, hidden) {
  var item = itemize(item_or_id);
  item.parent_sel = parent_sel;
  item.box = $( "<div class='ItemBox'></div>" ).appendTo( parent_sel );
  if ( hidden ) { 
    $( item.box ).addClass("Hidden");
  }
}
var destroyBox = function(item_or_id) {
  var item = itemize(item_or_id);
  if ( item.box ) {
    $( item.box ).remove();
    item.box = null;
  }
}
var moveBox = function(item_or_id, new_parent_sel) {
  var item = itemize(item_or_id);
  this.parent_sel = new_parent_sel;
  if ( item.box ) {
    $( item.box ).detach();
    $( item.box ).appendTo( new_parent_sel );
  }
}
var showBox = function(item_or_id) {
  var item = itemize(item_or_id);
  if ( item.box ) {
    $( item.box ).removeClass("Hidden");
  }
}
var hideBox = function(item_or_id) {
  var item = itemize(item_or_id);
  if ( item.box ) {
    $( item.box ).addClass("Hidden");
  }
}

// =========================================================================
function loadItem(item, replace) {
  var item_id = item["%ID"];
  if ( items.hasOwnProperty(item_id) && !replace ) {
    throw ("Item '" + item_id + "' already exists.");
  }
  items[item_id] = item;
}
function unloadItem(item_id) {
  delete items[item_id];
}
function getItemsByType(type) {
  var result = [];
  for (var i in items) {
    var item = items[i];
    if (item["%TYPE"] === type) {
      result.push(item);
    }
  }
  return result;
}
function getRelatedItems(type, relation, target_id) {
  var matched = [];
  var item;
  var keys = Object.getOwnPropertyNames(items);
  for ( var i = 0; i < keys.length; i++ ) {
    item = items[keys[i]];
    if ( item["%TYPE"] === type ) {
      matched += item;
    }
  }
  if ( matched.length > 0 ) {
    matched = matched.filter(function(elt) {
      item[relation] === target_id;
    });
  }
  return matched;
}
function isArray(value) {
  return value &&
    typeof value === 'object' &&
    value.constructor === Array;
}
function getLabel(item, item_type) {
  var label = "";
  var label_spec = display_specs[item_type]["label"];
  var segments = label_spec.split("&");
  for (var i = 0; i < segments.length; i++) {
    var segment = item[segments[i].trim()];
    label += (" " + segment);
  }
  return label.trim();
}

// =========================================================================
function toggleItemContent(evt) {
  $( evt.target ).siblings('.ItemContent').toggleClass('Hidden');
}
function focusItem(evt) {
  $( evt.target ).closest('.ItemBox').addClass('Current');
}
function unfocusItem(evt) {
  $( evt.target ).closest('.ItemBox').removeClass('Current');
}
function getSelectHandler(item, item_type) {
  var show_related_fun = {
    "project": showProjectRelated,
    "action": showActionRelated,
    "person": showContactRelated,
    "event": showEventRelated,
    "appointment": showAppointmentRelated,
    "note": showNoteRelated,
    "idea": showIdeaRelated
  }[item_type];
  return function(evt) {
    $( evt.target ).closest('.ItemBox').children('.ItemContent').removeClass('Hidden');
    show_related_fun(item);
  }
}
function showItemBox(item, item_type, parent_sel, is_subitem) {
  var klass;
  var label = getLabel(item, item_type); 
  var box_id = item_type + "-" + item["%ID"] + "-box"; 
  var item_box = $( "<div></div>" );
  var select_handler = getSelectHandler(item, item_type);
  if ( is_subitem ) {
    klass = "SubItemBox";
  } else {
    klass = "ItemBox";
  }
  $( parent_sel ).append( item_box );
  $( item_box ).addClass( klass );
  $( item_box ).attr("id", box_id);
  $( item_box ).append( "<h3 class='ItemLabel'>" + label + "</h3>" );
  $( item_box ).append( "<div class='ItemContent Hidden'></div>" );
  $( item_box ).children('.ItemLabel').bind("click", toggleItemContent);
  $( item_box ).children('.ItemLabel').bind("mouseenter", focusItem);
  $( item_box ).bind("mouseenter", focusItem);
  $( item_box ).children('.ItemLabel').bind("mouseleave", unfocusItem);
  $( item_box ).bind("mouseleave", unfocusItem);
  $( item_box ).bind("click", select_handler); 
  $( item_box ).children('.ItemLabel').bind("click", select_handler); 
  $( item_box ).children('.ItemContent').bind("click", select_handler); 
  return box_id;
}
function addOneLineField(container, label, data) {
  $( "<div></div>" ).appendTo( container )
    .append("<span class='FieldLabel'>" + label + ": </span>")
    .append("<span class='FieldValue'>" + data + "</span>");
}
function addMultiLineField(container, label, data) {
  var field_div = $( "<div></div>" ).appendTo( container )
    .append("<p class='FieldLabel'>" + label + ": </p>");
  if ( isArray(data) ) {
    for ( var i = 0; i < data.length; i++ ) {
      $( field_div ).append("<div class='FieldValue'>" + data[i] + "</div>");
    }
  } else {
    $( field_div ).append("<div class='FieldValue'>" + data + "</div>");
  }
}
function addMultiItemField(container, label, data) {
  var field_div = $( "<div></div>" ).appendTo( container )
    .append("<span class='FieldLabel'>" + label + ": </span>");
  var value_span = $( "<span class='FieldValue'></span>" );
  $( value_span ).appendTo( field_div );
  if ( isArray(data) ) {
    for ( var i = 0; i < data.length; i++ ) {
      var txt = $( value_span ).text(); 
      if ( txt === "" ) { 
        txt = data[i];
      } else {
        txt += ", " + data[i];
      }
      $( value_span ).text(txt);
    }
  } else {
    $( value_span ).text( data );
  }
}
function showProjectBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#projectSpace";
  }
  var box_id = showItemBox( item, "project", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
  if ( item.hasOwnProperty("description") ) {
    $( item_cont_sel ).append("<p class='ItemDescription'>" + item.description + "</p>");
  }
  if ( item.hasOwnProperty("deadline") ) {
    addOneLineField( item_cont_sel, "Deadline", item.deadline );
  }
  if ( item.hasOwnProperty("contacts") ) {
    addMultiLineField( item_cont_sel, "Contacts", item.contacts );
  }
  if ( item.hasOwnProperty("steps") ) {
    addMultiLineField( item_cont_sel, "Steps", item.steps );
  }
}
function showActionBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#toDoSpace";
  }
  var box_id = showItemBox( item, "action", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
  if ( item.hasOwnProperty("context") ) {
    addMultiItemField( item_cont_sel, "Context", item.context );
  }
}
function showEventBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#calendarSpace";
  }
  var box_id = showItemBox( item, "event", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
}
function showAppointmentBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#calendarSpace";
  }
  var box_id = showItemBox( item, "appointment", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
}
function showContactBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#contactSpace";
  }
  var box_id = showItemBox( item, "person", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
  if ( item.hasOwnProperty("email") ) {
    addMultiLineField(item_cont_sel, "Email Addresses", item.email);
  }
  if ( item.hasOwnProperty("phone") ) {
    addMultiLineField(item_cont_sel, "Phone Numbers", item.phone);
  }
}
function showIdeaBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#ideaSpace";
  }
  var box_id = showItemBox( item, "idea", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
}
function showNoteBox(item, parent_sel) {
  if ( parent_sel === undefined ) {
    parent_sel = "#noteSpace";
  }
  var box_id = showItemBox( item, "note", parent_sel );
  var item_cont_sel = "#" + box_id + " .ItemContent";
}
function showAsAncestor(item) {
  var item_type = item["%TYPE"];
  var parent_sel;
  switch ( item_type ) {
    case "project":
      parent_sel = "#ancProjectSpace";
      break;
    case "appointment":
      parent_sel = "#ancCalendarSpace";
      break;
    case "event":
      parent_sel = "#ancCalendarSpace";
      break;
    default:
      parent_sel = null;
  }
  if ( parent_sel ) {
  }
}

// =========================================================================
function showProjectRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#ancestorFrame" ).addClass("Hidden");
  $( "#descendantFrame" ).removeClass("Hidden");
  var contacts, steps;
  if ( item.hasOwnProperty("contacts") ) {
    var c;
    contacts = item["contacts"];
    if ( contacts ) {
      if ( isArray(contacts) ) {
        for (var i = 0; i < contacts.length; i++ ) {
          c = items[contacts[i]];
          showContactBox(c, "#desContactSpace" );
        }
      } else {
        c = items[contacts];
        showContactBox(c, "#desContactSpace" );
      }
      $( "#desContactSpace" ).removeClass("Hidden");
    }
  }
  if ( item.hasOwnProperty("steps") ) {
    var s;
    steps = item["steps"];
    if ( steps ) {
      if ( isArray(steps) ) {
        for (var i = 0; i < steps.length; i++ ) {
          s = items[steps[i]];
          showActionBox(s, "#desToDoSpace" );
        }
      } else {
        s = items[steps];
        showActionBox(s, "#desToDoSpace" );
      }
      $( "#desToDoSpace" ).removeClass("Hidden");
    }
  }
}
function showActionRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame" ).addClass("Hidden");
  $( "#ancestorFrame" ).removeClass("Hidden");
  var projects = getRelatedItems("project", "step", item["%ID"]);
  if ( projects.length > 0 ) {
    for ( var i = 0; i < projects.length; i++ ) {
      showProjectBox(projects[i], "#ancProjectSpace");
    }
    $( "#ancProjectSpace" ).removeClass("Hidden");
  }
}
function showContactRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame" ).addClass("Hidden");
  $( "#ancestorFrame" ).removeClass("Hidden");
  var projects = getRelatedItems("project", "step", item["%ID"]);
  if ( projects.length > 0 ) {
    for ( var i = 0; i < projects.length; i++ ) {
      showProjectBox(projects[i], "#ancProjectSpace");
    }
    $( "#ancProjectSpace" ).removeClass("Hidden");
  }
}
function showEventRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame" ).removeClass("Hidden");
}
function showAppointmentRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame" ).removeClass("Hidden");
}
function showNoteRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame" ).removeClass("Hidden");
}
function showIdeaRelated(item) {
  $( "#ancestorFrame, #descendantFrame" ).remove( ".ItemBox" );
  $( "#ancestorFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame .DisplaySpace" ).addClass("Hidden");
  $( "#descendantFrame" ).removeClass("Hidden");
}

// =========================================================================
function goToProject(evt) {
}

// =========================================================================
function showToDo() {
  $( ".DisplaySpace" ).addClass("Hidden");
  $( "#main_display" ).removeClass("Overview");
  $( "#toDoSpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#toDoTab" ).addClass("Selected");
}
function showCalendar() {
  $( ".DisplaySpace" ).addClass("Hidden");
  $( "#main_display" ).removeClass("Overview");
  $( "#calendarSpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#calendarTab" ).addClass("Selected");
}
function showProjects() {
  $( ".DisplaySpace" ).addClass("Hidden");
  $( "#main_display" ).removeClass("Overview");
  $( "#projectSpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#projectTab" ).addClass("Selected");
}
function showContacts() {
  $( ".DisplaySpace" ).addClass("Hidden");
  $( "#main_display" ).removeClass("Overview");
  $( "#contactSpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#selectedTab" ).addClass("Selected");
}
function showIdeas() {
  $( ".DisplaySpace" ).addClass("Hidden");
  $( "#main_display" ).removeClass("Overview");
  $( "#ideaSpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#ideaTab" ).addClass("Selected");
}
function showNotes() {
  $( ".DisplaySpace" ).addClass("Hidden");
  $( "#main_display" ).removeClass("Overview");
  $( "#noteSpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#noteTab" ).addClass("Selected");
}
function showOverview() {
  $( "#main_display" ).addClass("Overview");
  $( "#main_display > div.DisplaySpace" ).removeClass("Hidden");
  $( "#top_nav a" ).removeClass("Selected");
  $( "#overviewTab" ).addClass("Selected");
}
// =========================================================================
function newAction() {
  alert("newAction");
}
function newEvent() {
  alert("newEvent");
}
function newAppointment() {
  alert("newAppointment");
}
function newProject() {
  alert("newProject");
}
function newContact() {
  alert("newContact");
}
function newIdea() {
  alert("newIdea");
}
function newNote() {
  alert("newNote");
}
// =========================================================================
var test_projects = [{
  "%TYPE": "project",
  "%ID": "breed-mutant-seahorses",
  "%LABEL": "title",
  "title": "Breed Mutant Seahorses",
  "deadline": "2014-03-28T17:00:00",
  "contacts": "jane-morgan",
  "steps": ["build-tank", "catch-seahorses", "assign-to-tanks"] 
}];
var test_actions = [{
  "%TYPE": "action",
  "%ID": "build-tank",
  "%LABEL": "content",
  "content": "Build tank for seahorses",
  "context": ["lab"] 
},
{
  "%TYPE": "action",
  "%ID": "catch-seahorses",
  "%LABEL": "content",
  "content": "Catch us some seahorses",
  "context": ["atlantic","pacific","indian"] 
},
{
  "%TYPE": "action",
  "%ID": "assign-to-tanks",
  "%LABEL": "content",
  "content": "Put 'em in da tanks",
  "context": ["lab"] 
}];
var test_persons = [{
  "%TYPE": "person",
  "%ID": "jane-morgan",
  "%LABEL": "given-name & surname",
  "given-name": "Jane",
  "surname": "Morgan",
  "email": ["jane-morgan-work-email"] 
}];
var test_emails = [{
  "%TYPE": "email-address",
  "%ID": "jane-morgan-work-email",
  "label": "Work",
  "address": "dr.jane.v.morgan@sea-creature-research.com" 
}];

function showTestOverview() {
  showOverview();
  showProjectBox(test_projects[0]);
  showContactBox(test_persons[0]);
  for ( var i = 0; i < test_actions.length; i++ ) {
    showActionBox( test_actions[i] );
  }
}
function loadTestItems() {
  function loadArray(arr) {
    for ( var i = 0; i < arr.length; i++ ) { 
      loadItem(arr[i]);
    }
  }
  loadArray(test_projects);
  loadArray(test_actions);
  loadArray(test_persons);
  loadArray(test_emails);
}
function showTestProject() {
  loadTestItems();
  showProjects();
  showProjectBox(test_projects[0]); 
  showProjectRelated(test_projects[0]);
}
function showTestItems() {
  loadTestItems();
  showOverview();
  var keys = Object.getOwnPropertyNames(items);
  for ( var i = 0; i < keys.length; i++ ) {
    var item = items[keys[i]];
    switch ( item["%TYPE"] ) {
      case "project":
        showProjectBox(item);
        break;
      case "person":
        showContactBox(item);
        break;
      case "action":
        showActionBox(item);
        break;
    }
  }
}
