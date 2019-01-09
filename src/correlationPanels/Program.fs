

open System
open Aardvark.Base
open Aardvark.Application
//open Aardvark.Application.Slim
open Aardvark.UI
open Aardium

open Suave
open Suave.WebPart
open CorrelationDrawing
open Aardvark.Application.WinForms


[<EntryPoint; STAThread>]
let main argv = 
    
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    

    Ag.initialize()
    Aardvark.Init()
    Aardium.init()
    use app = new OpenGlApplication()
    

    // use can use whatever suave server to start you mutable app. 
    // startServerLocalhost is one of the convinience functions which sets up 
    // a server without much boilerplate.
    // there is also WebPart.startServer and WebPart.runServer. 
    // look at their implementation here: https://github.com/aardvark-platform/aardvark.media/blob/master/src/Aardvark.Service/Suave.fs#L10
    // if you are unhappy with them, you can always use your own server config.
    // the localhost variant does not require to allow the port through your firewall.
    // the non localhost variant runs in 127.0.0.1 which enables remote acces (e.g. via your mobile phone)

    // CORRELATIONS
    WebPart.startServerLocalhost 4321 [ 
        MutableApp.toWebPart' app.Runtime false (Pages.start app.Runtime)
        Suave.Files.browseHome
    ]  

    //TEST
    //WebPart.startServerLocalhost 4321 [ 
    //    MutableApp.toWebPart app.Runtime (App.start Test.App.app)
    //    Suave.Files.browseHome
    //]  

    Aardium.run {
        url "http://localhost:4321/"
        width 1024
        height 768
        debug true
        menu true
    }

    //use ctrl = new AardvarkCefBrowser()
    //ctrl.Dock <- DockStyle.Fill
    //form.Controls.Add ctrl
    //ctrl.StartUrl <- "http://localhost:4321/"
    //ctrl.ShowDevTools()
    //form.Text <- "Examples"
    //form.Icon <- Icons.aardvark 

    //Application.Run form
    0 


//[<EntryPoint>]
//let main args =
//    Ag.initialize()
//    Aardvark.Init()
//    Aardium.init()

//    let app = new HeadlessVulkanApplication(true)

//    WebPart.startServer 4321 [
//        MutableApp.toWebPart' app.Runtime false (App.start App.app)
//    ]
    
//    Aardium.run {
//        title "Aardvark rocks \\o/"
//        width 1024
//        height 768
//        url "http://localhost:4321/"
//    }

//    0
