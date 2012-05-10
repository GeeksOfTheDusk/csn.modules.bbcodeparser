package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("[i][b]Your new application is ready[/b][/i]"))
  }
  
}