# heist-async: Adding asynchronous updates of pages using heist.

## This provides four splices, exported in heistAsyncSplices

**\<activate-async/\>** - This brings in the required javascript - mainly, the three small javascript libraries (in total weighing 12k) and the cade to make the splice replacement work (in \<1k). Minified (with uglifyjs) and regular versions are available in the js folder, and for production, serving those files would be preferable to including inline (as that way they could be cached), and implementations of heist-async.js using bigger frameworks (jQuery, prototype, etc) would be appreciated. Ideally they would be made available both as standalone files (ie, heist-async-jquery.js) and as splices (\<activate-async-jquery/\>). This splice will only run once (the first place it is called), so feel free to include it in various places throughout your templates, if that suits you needs better.

**\<a-async\>** - this is a regular \<a\> tag, except that it will add an extra attribute (rel=async) that will allow the javascript to capture the clicks, so it can be used any way a regular tag would be used. Because of this, it will automatically fall back to functioning as a normal link in the absence of javascript support. When it is clicked, the server is sent a request at the url provided in the href attribute, and then the response (which should be a list of top-level \<div-async\>s) is used to replace the corresponding \<div-async\>s on the page.
  
**\<form-async\>** - analogously to \<a-async\>, this is a normal \<form\> tag that has an extra attribute added (data-async=1) to allow the javascript to capture the submits. Again, this means that it will fall back to a normal submit in the absence of javascript support.
  
**\<div-async name="something"\>** - this will result in a normal div tag that has an extra attribute identifying it as a div that can be replaced asynchronously (it is, data-splice-name). When you generate the original page, you put in \<div-async\>'s in all the places where you may want to replace content (and they do not have to be empty; in the case of pagination, they could contain the first page's worth of data), and then in the responses to the \<a-async\> or \<form-async\> request to the server, return \<div-async\>'s as the top level elements and the existing ones (if they exist) will be replaced by the now ones.

**\<div-async-append name="something"\>** - this will append the contents inside an existing \<div-async-append\>. 

## The idea behind this:
The immediate inspiration for this is Facebook's Primer (and some of the code is derived from what they have made available), and more specifically the idea of controlling what content to replace client side on the server. So, when a client makes a request (via \<a-async\> or \<form-async\>) the server decides what \<div-async\>'s to pass back and therefore what content to replace client on the client. Depending on factors that the client could know nothing about, these div's could be different. The server could also choose to hide a specific div by passing back a \<div-async style="display:none"/\>. Additionally, this means that the rendering code can be identical, because if you have the code to render a given part of a page in a separate template, if it is a non-async request then you can render the whole page, and if it is an async request you can render just that part, without having to change how you are rendering, and without having to duplicate anything.

## Other thoughts:
This is really early software, and I have intentionally tried to keep it really minimal, to see how far it can go with just these three tags. There are a lot of other possibilities, but I'm not sure if they are actually necessary, and I wanted to stay on the minimal end to begin with.

## Usage example 

(this is using Snap, though the library in no way depends upon Snap):

test.tpl:

    <html>
      <head>
        <activate-async/>
      </head>
      <body>
        <a-async href="/ajax/a">Test link</a-async>
    
        <form-async action="/ajax/form">
        <input name="test" type="text"/>
        <input type="submit" value="submit"/>
        </form-async>
    
        <div-async name="new-entry" id="something">Starting value</div-async>
      </body>
    </html>

testa.tpl:

    <div-async name="new-entry">New value</div-async>

testform.tpl:

    <div-async name="new-entry">New value with data: <t/></div-async>

handler (this is a partial example, but it should be obvious how to apply it):

    import qualified  Data.Text.Encoding as TE
    import            Snap.Types
    import            Heist.Splices.Async
    
    testForm = do t <- getParam "test"
                  heistLocal (bindString "t" (maybe "Nothing" TE.decodeUtf8 t)) $ render "testform"
    
    site = route [ ("/test",      heistLocal (bindSplices heistAsyncSplices) $ render "test")
                 , ("/ajax/a",    heistLocal (bindSplices heistAsyncSplices) $ render "testa")
                 , ("/ajax/form", heistLocal (bindSplices heistAsyncSplices) $ testForm)
                 ]