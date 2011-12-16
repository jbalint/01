// see about:config with prefix jess.springer
// TODO idea; for LNCS, detect Vol # and title for dir name, new menu item?
// Also, better to put your settings to close the download window when complete
// for some reason the download window re-opens all the time with this program
// TODO handle the same volume num twice?

// Configuration options
// jess.springer.fileprefix - default file prefix for single-volume downloads
// jess.springer.downloadwait - number of secs to let a download before init'ing another
// jess.springer.logfile - logfile name, if blank, no logging
// jess.springer.dirprefix - default dir prefix for whole journal downloads

// dont go to the 'Next' page inside a journal
const debugDontNextPage = false;

///////////////////////////////////////////////////////////
// log a message if the logging file is configured
///////////////////////////////////////////////////////////
function log(str) {
  // get log file path from prefs
  var prefs = Components.classes["@mozilla.org/preferences-service;1"]
    .getService(Components.interfaces.nsIPrefBranch);
  var logfile;
  if(prefs.getPrefType("jess.springer.logfile") == prefs.PREF_STRING)
    logfile = prefs.getCharPref("jess.springer.logfile");
  else
    return;

  // open the log file, and create if not exists
  var fileobj = Components.classes['@mozilla.org/file/local;1']
    .createInstance(Components.interfaces.nsILocalFile);
  fileobj.initWithPath(logfile);
  //fileobj.initWithPath("U:\\Springer\\dl.log");
  if(!fileobj.exists())
    fileobj.createUnique(Components.interfaces.nsIFile.NORMAL_FILE_TYPE, 600);
  //var filestr = Components.classes['@mozilla.org/network/safe-file-output-stream;1']
  var filestr = Components.classes['@mozilla.org/network/file-output-stream;1']
    .createInstance(Components.interfaces.nsIFileOutputStream);
  //var flags = 0x02 | 0x08 | 0x10; // 0x20 = truncate, 0x10 = append
  var flags = 0x04 | 0x10; // 0x04 = r/w, 0x20 = truncate, 0x10 = append
  filestr.init(fileobj, flags, 0600, 0);

  // write the log message and close
  filestr.write(str, str.length);
  filestr.write("\n", 1);

  filestr.flush();
  filestr.close();
}

///////////////////////////////////////////////////////////
// Download a file using the Firefox Download Manager
// srcpath = HTTP URL,
// destpath = FILE PATH,
// name = Some name (used to display in dlmgr)
///////////////////////////////////////////////////////////
function downloadfile(srcpath, destpath, name) {
  log("Downloading " + srcpath + " to " + destpath + " as " + name);

  var io = Components.classes["@mozilla.org/network/io-service;1"]
    .getService(Components.interfaces.nsIIOService);
  var src = io.newURI(srcpath, null, null);
  var local = Components.classes["@mozilla.org/file/local;1"]
    .getService(Components.interfaces.nsILocalFile);
  var f2 = local;//.clone();
  f2.initWithPath(destpath);
  var f3 = f2.clone();
  var dest = io.newFileURI(f3);

  var progressPersist = Components.classes['@mozilla.org/embedding/browser/nsWebBrowserPersist;1']
    .createInstance(Components.interfaces.nsIWebBrowserPersist);
  var nsIWBP = Components.interfaces.nsIWebBrowserPersist;
  var flags = nsIWBP.PERSIST_FLAGS_AUTODETECT_APPLY_CONVERSION |
              nsIWBP.PERSIST_FLAGS_REPLACE_EXISTING_FILES |
              nsIWBP.PERSIST_FLAGS_BYPASS_CACHE;
  progressPersist.persistFlags = flags;

  var trans = Components.classes["@mozilla.org/transfer;1"]
    .createInstance(Components.interfaces.nsITransfer);
  //trans.init(src, dest, name, null, null, null, null);
  trans.init(src, dest, name, null, null, null, progressPersist);
  progressPersist.progressListener = trans;
  var referer = Components.classes["@mozilla.org/network/standard-url;1"]
                               .createInstance(Components.interfaces.nsIURI);
  progressPersist.saveURI(src, null, referer, null, null, f3);
}

///////////////////////////////////////////////////////////
// escape the backslashes, needed for using setTimeout()
///////////////////////////////////////////////////////////
function escape_backslashes(filename) {
  var pcs = filename.split("\\");
  var fn = "";
  for(var i = 0; i < pcs.length - 1; ++i)
  {
    // where was one, now is two
    fn += pcs[i] + "\\\\";
  }
  return fn + pcs[pcs.length - 1];
}

///////////////////////////////////////////////////////////
// clear out the download manager
///////////////////////////////////////////////////////////
function cleanup_downloadmanager() {
  var dlmgr = Components.classes["@mozilla.org/download-manager;1"]
    .getService(Components.interfaces.nsIDownloadManager);
  dlmgr.cleanUp();
}

///////////////////////////////////////////////////////////
// strip off the spaces and crap (for the matter titles)
///////////////////////////////////////////////////////////
function stripspaces(str1)
{
  while(str1.length > 0 &&
    (str1.indexOf(' ') == 0 ||
     str1.indexOf('\n') == 0 ||
     str1.indexOf('\t') == 0))
  {
    str1 = str1.substr(1, str1.length - 1);
  }
  while(str1.length > 0 &&
    (str1.lastIndexOf(' ') == (str1.length - 1) ||
     str1.lastIndexOf('\n') == (str1.length - 1) ||
     str1.lastIndexOf('\t') == (str1.length - 1)))
  {
    str1 = str1.substr(0, str1.length - 1);
  }
  return str1;
}

///////////////////////////////////////////////////////////
// Format a number with zero-padded in the beginning to be
// at least 'len' characters
///////////////////////////////////////////////////////////
function formatnum(num, len)
{
  var numstr = "" + num
  if(numstr.length < len)
  {
    var l = len - numstr.length;
    for(var x = 0; x < l; ++x)
    {
      numstr = "0" + numstr;
    }
  }
  return numstr;
}

///////////////////////////////////////////////////////////
// Check if a directory exists, if not create it
// TODO is there a better? Much better? way to do this?
// maybe createInstance() instead of getService()?
///////////////////////////////////////////////////////////
function checkAndCreate(dir) {
  var local = Components.classes["@mozilla.org/file/local;1"]
    .getService(Components.interfaces.nsILocalFile);
  var f2 = local;
  f2.initWithPath(dir);
  var f3 = f2.clone();
  if(!f3.exists() || !f3.isDirectory()) {
    f3.create(Components.interfaces.nsIFile.DIRECTORY_TYPE, 0777);
  }
}

///////////////////////////////////////////////////////////
// Get the class name for a given element
///////////////////////////////////////////////////////////
function getClassName(el) {
  var cl = el.attributes.getNamedItem("class")
  if(cl != null)
    return cl.value;
  else
    return null;
}

///////////////////////////////////////////////////////////
// Singleton/global context for page-mapping
// client code must set 'next' for the next page and
// is free to user 'userdata' for whatever it wants
///////////////////////////////////////////////////////////
var mapcontext =
  {in_progress: false,
   page_func:   null,
   final_func:  null,
   next:        null,
   prev:        "",
   userdata:    null}

///////////////////////////////////////////////////////////
// The main workhouse of page-mapping. Called when a page
// is loading, does this step of the map and loads a new
// page is one is given.
///////////////////////////////////////////////////////////
function map_Pages_onLoad(ev) {
  if(!mapcontext.in_progress)
    return;

  // somehow we can get the same load event twice
  var page = content.document.baseURI;
  if(page == mapcontext.prev)
    return;
  else
    mapcontext.prev = page;

  mapcontext.page_func(content.document, mapcontext);
  if(mapcontext.next != null)
  {
    var nextpage = mapcontext.next;
    mapcontext.next = null;
    openUILink(nextpage, null, false, true);
  }
  else
  {
    // remove the listener
    var appcontent = document.getElementById("appcontent");
    appcontent.removeEventListener("load", map_Pages_onLoad, true);
    // done
    mapcontext.in_progress = false;
    // call the client code finalizer
    if(mapcontext.final_func != null)
      mapcontext.final_func(mapcontext);
  }
}

///////////////////////////////////////////////////////////
// Initialize a page-mapping.
///////////////////////////////////////////////////////////
function map_Pages(init_func, page_func, final_func, nextpage, userdata) {
  // if we're not "in a map", start it
  if(mapcontext.in_progress)
  {
    alert("Map context already in progress");
    return;
  }
  mapcontext.in_progress = true;
  mapcontext.page_func = page_func;
  mapcontext.final_func = final_func;
  mapcontext.userdata = userdata;

  // add listener
  var appcontent = document.getElementById("appcontent");
  appcontent.addEventListener("load", map_Pages_onLoad, true);

  if(init_func != null)
    init_func(mapcontext);

  openUILink(nextpage, null, false, true);
}

///////////////////////////////////////////////////////////
// Menu-handler for downloading a whole journal
///////////////////////////////////////////////////////////
function jnl_download() {
  // check prefs for default dir prefix
  var prefs = Components.classes["@mozilla.org/preferences-service;1"]
    .getService(Components.interfaces.nsIPrefBranch);
  var dirprefix = "";
  if(prefs.getPrefType("jess.springer.dirprefix") == prefs.PREF_STRING){
    dirprefix = prefs.getCharPref("jess.springer.dirprefix");
  }

  // prompt for an updated value
  do {
    dirprefix = prompt("Dir prefix (need trailing slash):", dirprefix);
    if(dirprefix == null) // dialog cancelled
      return;
    if(dirprefix.lastIndexOf("\\") == (dirprefix.length - 1))
      alert("Must NOT include trailing slash!");
  } while(dirprefix.lastIndexOf("\\") == (dirprefix.length - 1));

  var userdata = {volnum: 0,
                  issueq: new Array(),
                  dirprefix: dirprefix};
  map_Pages(jnl_init, jnl_page, jnl_get_issue, content.document.baseURI, userdata);
}

function jnl_init(ctx) {
}

function jnl_page(el, ctx) {
  if(el == null)
    return;

  var ud = ctx.userdata;

  for(var x = 0; x < el.childNodes.length; ++x)
  {
    var n = el.childNodes[x];

    // detect volume number headings
    if(n.nodeName == "DIV" && getClassName(n) == "listItemName")
    {
      var text = stripspaces(n.innerHTML);
      if(text.indexOf("Volume ") == 0)
      {
        var len = "Volume ".length
        ud.volnum = text.substr(len, text.length - len)
      }
    }
    else if(n.nodeName == "A")
    {
      var text = stripspaces(n.text);
      if(text == "Next Page")
      {
        // set a variable and check it after this
        // whole page has been processed
        ctx.next = n.href;
      }
      else if(text.indexOf("Number ") == 0 || text.indexOf("Numbers ") == 0)
      {
        var start = text.indexOf(" ") + 1;
        var len = text.indexOf(" / ") - start;
        var issuenum = text.substr(start, len);

        // add to queue for later processing
        var issue = { href: n.href, volnum: ud.volnum,
          issuenum: issuenum };
        log("Queueing Issue " + issue.volnum + "-" + issue.issuenum + " (" + issue.href + ")");
        ud.issueq[ud.issueq.length] = issue;
      }
    }

    jnl_page(n, ctx);
  }
}

function jnl_get_issue(ctx) {
  var iq = ctx.userdata.issueq;
  var issue = iq[0];

  // Done!
  if(issue == null)
    return;

  log("Processing Issue " + issue.volnum + "-" + issue.issuenum + " (" + issue.href + ")");

  var fileprefix = ctx.userdata.dirprefix + " " + issue.volnum + "-" + issue.issuenum;
  // the same issue number could be encountered twice
  // TODO still need to double-check/test this
  if((fileprefix + "\\") == ctx.userdata.fileprefix)
    fileprefix += "-2";
  fileprefix += "\\";
  checkAndCreate(fileprefix);

  var userdata = {get_title: 0,
                  title: "",
                  docnum: 0,
                  articleq: new Array(),
                  fileprefix: fileprefix,
                  dirprefix: ctx.userdata.dirprefix,
                  // pop the first element
                  issueq: iq.slice(1, iq.length)};
  var jnl_vol_finish = function(ctx) { vol_finish(ctx); jnl_get_issue(ctx); }
  map_Pages(vol_init, vol_page, jnl_vol_finish, issue.href, userdata);
}

///////////////////////////////////////////////////////////
// Menu-handler for downloading a single volume
///////////////////////////////////////////////////////////
function vol_download() {
  // check prefs for default file prefix
  var prefs = Components.classes["@mozilla.org/preferences-service;1"]
    .getService(Components.interfaces.nsIPrefBranch);
  var fileprefix = "";
  if(prefs.getPrefType("jess.springer.fileprefix") == prefs.PREF_STRING){
    fileprefix = prefs.getCharPref("jess.springer.fileprefix");
  }

  // prompt for an updated value
  do {
    fileprefix = prompt("File prefix (need trailing slash):", fileprefix);
    if(fileprefix == null) // dialog cancelled
      return;
    if(fileprefix.lastIndexOf("\\") != (fileprefix.length - 1))
      alert("Must include trailing slash!");
  } while(fileprefix.lastIndexOf("\\") != (fileprefix.length - 1));
  checkAndCreate(fileprefix);

  var userdata = {get_title: 0,
                  title: "",
                  docnum: 0,
                  articleq: new Array(),
                  fileprefix: fileprefix};

  map_Pages(vol_init, vol_page, vol_finish, content.document.baseURI, userdata);
}

function vol_init(ctx) {
  // nothing?
}

function vol_page(el, ctx) {
  if(el == null)
    return;

  var ud = ctx.userdata;

  for(var x = 0; x < el.childNodes.length; ++x)
  {
    var n = el.childNodes[x];

    if(n.nodeName == "DIV" && getClassName(n) == "listItemName")
    {
      ud.get_title = 1;
      // special case for Front/Back Matter
      var text = stripspaces(n.childNodes[0].nodeValue);
      if(text.substr(-6, 6) == "Matter")
      {
        ud.title = text;
        ud.get_title = 0;
      }
    }
    else if(n.nodeName == "A")
    {
      if(ud.get_title)
      {
        ud.get_title = 0;
        ud.title = n.text;
      }
      else if(n.text == "Next")
      {
        ctx.next = n.href;
      }
      else if(n.href.substr(-4, 4) == ".pdf")
      {
        var filename
        if(ud.title == "Front Matter")
          filename = "front_matter.pdf";
        else if(ud.title == "Back Matter")
          filename = "back_matter.pdf";
        else
        {
          filename = formatnum(ud.docnum, 3) + ".pdf";
          ud.docnum++;
        }

        ud.articleq[ud.articleq.length] =
          {filename: filename, href: n.href, title: ud.title};
      }
    }
    vol_page(n, ctx);
  }
}

function vol_finish(ctx) {
  var aq = ctx.userdata.articleq;
  var secs = 0;
  var incsecs = 4; // default

  // read preference override
  var prefs = Components.classes["@mozilla.org/preferences-service;1"]
    .getService(Components.interfaces.nsIPrefBranch);
  if (prefs.getPrefType("jess.springer.downloadwait") == prefs.PREF_INT){
    incsecs = prefs.getIntPref("jess.springer.downloadwait");
  }

  for(var i = 0; i < aq.length; ++i)
  {
    var article = aq[i];
    var filename = ctx.userdata.fileprefix + article.filename;
    var cmd = "downloadfile(\"" + article.href + "\", \"" + escape_backslashes(filename) + "\", \"DL\")";

    log("Queueing download (" + article.href + ") to (" + filename + ") as (" + article.title + ")");
    setTimeout(cmd, secs * 1000); // schedule download
    secs += incsecs;

    // TODO configurable?
    if(i % 5 == 0)
      setTimeout("cleanup_downloadmanager()", secs * 1000);
  }
}

var test1 = {
  showContextMenu: function(event) {
    // show or hide the menuitem based on what the context menu is on
    // see http://kb.mozillazine.org/Adding_items_to_menus
    document.getElementById("context-test1").hidden = gContextMenu.onImage;
  },

  onLoad: function() {
    // initialization code
    this.initialized = true;
    this.strings = document.getElementById("test1-strings");
    document.getElementById("contentAreaContextMenu")
            .addEventListener("popupshowing", function(e) { test1.showContextMenu(e); }, false);
  }
};

window.addEventListener("load", function(e) { test1.onLoad(e); }, false);

// vim:ts=2 sw=2 et
