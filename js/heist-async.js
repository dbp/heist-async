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
      } else if (elements[i].nodeName == "DIV" && elements[i].hasAttribute("data-append-name")) {
        var rep = qwery("div[data-append-name=" + elements[i].getAttribute("data-append-name") + "]");
        if (rep && rep[0] && elements[i].childNodes) {
          for(n=0; n < elements[i].childNodes.length; n++) {
            if (elements[i].childNodes[n].nodeType == 1) {
              rep[0].appendChild(elements[i].childNodes[n]);
            }
          }
        }
      }
    };
    // now run any included javascript
    var scripts, scriptsFinder=/<script[^>]*>([\s\S]+)<\/script>/gi;
    while(scripts=scriptsFinder.exec(resp))
    {
       eval(scripts[1]);
    }
  };

  // Listeners for most common interations                                            
  htm.onclick = function(e) {
    e = e || window.event;
    lct = e.target || e.srcElement;

    if (lct && lct.nodeName === "BUTTON") {
      bonzo(lct).addClass("processing");
      return;
    }

    var elem = nearest(lct, 'A') || htm,
        href = elem.href;

    switch (elem.rel) {
    case 'async':
    case 'async-post':
      if (elem.getAttribute("data-loading-div")) {
        if (qwery(elem.getAttribute("data-loading-div"))[0]) {
          qwery(elem.getAttribute("data-loading-div"))[0].innerHTML = "<div class='loading'></div>";
        }
      };
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
    if (elem.getAttribute("data-loading-div")) {
      if (qwery(elem.getAttribute("data-loading-div"))[0]) {
        qwery(elem.getAttribute("data-loading-div"))[0].innerHTML = "<div class='loading'></div>";
      }
    }
    reqwest({
          url: elem.getAttribute('action') || "",
          data: reqwest.serialize(elem),
          method: elem.getAttribute('method') || 'post',
          type: 'html',
          success: replace_splices
        });
    return false;
  };
}();
