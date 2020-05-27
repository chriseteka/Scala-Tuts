// @GENERATOR:play-routes-compiler
// @SOURCE:C:/Users/Chris_Eteka/Documents/SCALA PROJECTS/scala-api-trial/conf/routes
// @DATE:Wed May 06 09:53:58 WAT 2020

package router

import play.core.routing._
import play.core.routing.HandlerInvokerFactory._

import play.api.mvc._

import _root_.controllers.Assets.Asset

class Routes(
  override val errorHandler: play.api.http.HttpErrorHandler, 
  // @LINE:1
  v1_post_PostRouter_0: v1.post.PostRouter,
  val prefix: String
) extends GeneratedRouter {

   @javax.inject.Inject()
   def this(errorHandler: play.api.http.HttpErrorHandler,
    // @LINE:1
    v1_post_PostRouter_0: v1.post.PostRouter
  ) = this(errorHandler, v1_post_PostRouter_0, "/")

  def withPrefix(addPrefix: String): Routes = {
    val prefix = play.api.routing.Router.concatPrefix(addPrefix, this.prefix)
    router.RoutesPrefix.setPrefix(prefix)
    new Routes(errorHandler, v1_post_PostRouter_0, prefix)
  }

  private[this] val defaultPrefix: String = {
    if (this.prefix.endsWith("/")) "" else "/"
  }

  def documentation = List(
    prefixed_v1_post_PostRouter_0_0.router.documentation,
    Nil
  ).foldLeft(List.empty[(String,String,String)]) { (s,e) => e.asInstanceOf[Any] match {
    case r @ (_,_,_) => s :+ r.asInstanceOf[(String,String,String)]
    case l => s ++ l.asInstanceOf[List[(String,String,String)]]
  }}


  // @LINE:1
  private[this] val prefixed_v1_post_PostRouter_0_0 = Include(v1_post_PostRouter_0.withPrefix(this.prefix + (if (this.prefix.endsWith("/")) "" else "/") + "v1/posts"))


  def routes: PartialFunction[RequestHeader, Handler] = {
  
    // @LINE:1
    case prefixed_v1_post_PostRouter_0_0(handler) => handler
  }
}
