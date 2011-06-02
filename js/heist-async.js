!function() {
  // this code based on facebook's Primer, provided to world at https://gist.github.com/376039
  var doc     = document,
      htm     = doc.documentElement,
      lct     = null,   // last click target                                              
      nearest = function(elm, tag) {
        while (elm && elm.nodeName != tag) {
          elm = elm.parentNode;
        }
        return elm;
  };

  var replace_splices = function (resp) {
    var div = document.createElement('div');
    div.innerHTML = resp;
    var elements = div.childNodes;
    for (i=0; i<elements.length; i++) {
      if (elements[i].nodeName == "DIV" && elements[i].hasAttribute("data-splice-name")) {
        var rep = qwery("div[data-splice-name=" + elements[i].getAttribute("data-splice-name") + "]");
        if (rep && rep[0]) {
          rep[0].parentNode.replaceChild(elements[i],rep[0]);
        }
      }
    };
  };

  // Listeners for most common interations                                            
  htm.onclick = function(e) {
    e = e || window.event;
    lct = e.target || e.srcElement;

    var elem = nearest(lct, 'A') || htm,
        href = elem.href;

    switch (elem.rel) {
    case 'async':
    case 'async-post':
      reqwest({
        url:href, 
        type: 'html',
        success: replace_splices
      });
      break;
    default:
      return;
    }

    return false;
  };

  htm.onsubmit = function(e) {
    e = e || window.event;
    var elem = e.target || e.srcElement;

    if (!elem || elem.nodeName !== 'FORM' || !elem.getAttribute('data-async')) {
      return;
    }
    reqwest({
          url: elem.getAttribute('action') || "",
          data: reqwest.serialize(elem),
          method: 'post',
          type: 'html',
          success: replace_splices
        });
    return false;
  };
}();
