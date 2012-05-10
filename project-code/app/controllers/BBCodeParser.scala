package controllers

import scala.util.matching._


object BBCodeParser {
    implicit def stringToHtml(s: String) = new {
      def escape = {
        var html = s.replace("\n", "<br/>")
          .replace("[b]", "<b>").replace("[/b]", "</b>")
          .replace("[i]", "<i>").replace("[/i]", "</i>")
          .replace("[u]", "<u>").replace("[/u]", "</u>")
          .replace("[center]", "<center>").replace("[/center]", "</center>")

        html = replaceFontWithRegex(html)
        html = replaceUrlAndImg(html)
        html = replaceList(html)

        html.replace("\n", "<br/>")
      }
    }

    private def replaceFontWithRegex(s: String) = {
      var re = s
      val fontRegex = new Regex("\\[font=([^\\]]+)\\]", "font")
      re = fontRegex replaceAllIn (re, m => "<font face=\"" + m.group("font") + "\">") replace ("[/font]", "</font>")

      val colorRegex = new Regex("\\[color=([^\\]]+)\\]", "color")
      re = colorRegex replaceAllIn (re, m => "<font color=\"" + m.group("color") + "\">") replace ("[/color]", "</font>")

      val sizeRegex = new Regex("\\[size=([^\\]]+)\\]", "size")
      re = sizeRegex replaceAllIn (re, m => "<font size=\"" + m.group("size") + "\">") replace ("[/size]", "</font>")
      re
    }

    private def replaceUrlAndImg(s: String) = {
      var re = s
      val imageRegex = new Regex("\\[img\\](.*?)\\[/img\\]", "image")
      val urlRegex = new Regex("\\[url=?(.*?)\\](.*?)\\[/url\\]", "url", "text")
      re = imageRegex replaceAllIn (re , m => "<img source=\"" + m.group("image") + "\" alt=\"An image\"/>")
      re = urlRegex replaceAllIn (re, m => {
        var prot = "http://"
        if(!m.group("url").isEmpty) {
          if(m.group("url").startsWith("http://"))
            prot = ""
          "<a href=\"" + prot + m.group("url") + "\">" + m.group("text") + "</a>"
        }
        else {
          if(m.group("text").startsWith("http://"))
            prot = ""
          "<a href=\"" + prot + m.group("text") + "\">" + m.group("text") + "</a>"
        }
      })
      re
    }

    private def replaceList(s: String) = {
      var re = s
      val listRegex = new Regex("""(?s)\[list=?([1ia])?\](.*?)\[/list\]""", "opt", "content")
      re = listRegex replaceAllIn (re, { m =>
        if( m.group("opt")== null || m.group("opt").isEmpty) {
          "<ul>" + m.group("content") + "</ul>"
        } else {
          if(m.group("opt") == "1") {
            "<ol>" + m.group("content") + "</ol>"
          } else {
            "<ol type=\"" + m.group("opt") + "\">" + m.group("content") + "</ol>"
          }
        }
      })

      val bulletRegex = new Regex("""\[\*\](.+)""", "content")

      bulletRegex replaceAllIn (re, m => "<li>" + m.group("content") + "</li>")
    }
}
