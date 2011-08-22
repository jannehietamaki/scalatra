package org.scalatra

import javax.servlet.http.{HttpSession, HttpServletRequest}
import javax.servlet.ServletContext
import scala.util.DynamicVariable
import util.MultiMap

class RouteExecutor(
                  app: ScalatraKernel,
                  methodNotAllowedHandler: Set[HttpMethod] => Any,
                  errorHandler: ErrorHandler,
                  notFoundHandler: ScalatraKernel.Action,
                  renderResponseBody: Any => Unit,
                  renderHaltException: ScalatraKernel#HaltException => Unit,
                  multiParams: DynamicVariable[MultiMap]) {

  protected implicit def requestWrapper(r: HttpServletRequest) = RichRequest(r)
  protected implicit def sessionWrapper(s: HttpSession) = new RichSession(s)
  protected implicit def servletContextWrapper(sc: ServletContext) = new RichServletContext(sc)


  def execute: Any = {
    try {
      runFilters(app.routes.beforeFilters)
      val actionResult = runRoutes(app.routes(app.request.method)).headOption
      actionResult orElse matchOtherMethods() getOrElse notFoundHandler()
    }
    catch {
      case e: ScalatraKernel#HaltException => renderHaltException(e)
      case e => errorHandler(e)
    }
    finally {
      runFilters(app.routes.afterFilters)
    }
  }



  protected def matchOtherMethods(): Option[Any] = {
    val allow = app.routes.matchingMethodsExcept(app.request.method)
    if (allow.isEmpty) None else Some(methodNotAllowedHandler(allow))
  }

  protected def runFilters(filters: Traversable[Route]) =
    for {
      route <- filters
      matchedRoute <- route()
    } invoke(matchedRoute)

  protected def runRoutes(routes: Traversable[Route]) =
    for {
      route <- routes.toStream // toStream makes it lazy so we stop after match
      matchedRoute <- route()
      actionResult <- invoke(matchedRoute)
    } yield actionResult

  protected def invoke(matchedRoute: MatchedRoute) =
    multiParams.withValue(app.multiParams ++ matchedRoute.multiParams) {
      try {
        Some(matchedRoute.action())
      }
      catch {
        case e: ScalatraKernel#PassException => None
      }
    }
}