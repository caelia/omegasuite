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
/*
function getItemByID(id) {
  var matched = items.filter(function(item) {
    return item["%ID"] === id;
  });
  switch ( matched.length ) {
    case 0:
      return null;
    case 1:
      return matched[0];
    default:
      throw "Error: duplicate item IDs";
  }
}
*/
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
function showItemBox(item, item_type, parent_sel, is_subitem) {
  var klass;
  if ( is_subitem ) {
    klass = "SubItemBox";
  } else {
    klass = "ItemBox";
  }
  var label = getLabel(item, item_type); 
  var box_id = item_type + "-" + item["%ID"] + "-box"; 
  var item_box = $( "<div></div>" );
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
}
function showContactRelated(item) {
}
function showEventRelated(item) {
}
function showAppointmentRelated(item) {
}
function showNoteRelated(item) {
}
function showIdeaRelated(item) {
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
function showTestProject() {
  function loadArray(arr) {
    for ( var i = 0; i < arr.length; i++ ) { 
      loadItem(arr[i]);
    }
  }
  loadArray(test_projects);
  loadArray(test_actions);
  loadArray(test_persons);
  loadArray(test_emails);
  showProjects();
  showProjectBox(test_projects[0]); 
  showProjectRelated(test_projects[0]);
}
