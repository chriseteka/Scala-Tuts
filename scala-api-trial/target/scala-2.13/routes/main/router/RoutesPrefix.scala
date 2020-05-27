// @GENERATOR:play-routes-compiler
// @SOURCE:C:/Users/Chris_Eteka/Documents/SCALA PROJECTS/scala-api-trial/conf/routes
// @DATE:Wed May 06 09:53:58 WAT 2020


package router {
  object RoutesPrefix {
    private var _prefix: String = "/"
    def setPrefix(p: String): Unit = {
      _prefix = p
    }
    def prefix: String = _prefix
    val byNamePrefix: Function0[String] = { () => prefix }
  }
}
