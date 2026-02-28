package dndbuilder.common.pdf

import org.scalajs.dom

import scala.scalajs.js

object PdfLib:

  private def pdfLib: Option[js.Dynamic] =
    val lib = js.Dynamic.global.selectDynamic("PDFLib")
    if js.typeOf(lib) == "undefined" then None else Some(lib)

  def loadFromUrl(url: String)(onLoaded: js.Dynamic => Unit)(onError: String => Unit): Unit =
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    dom.console.log("[PdfLib] loadFromUrl called, pdfLib present: " + pdfLib.isDefined)
    pdfLib match
      case None =>
        dom.console.error("[PdfLib] PDFLib not on window - add script to index.html")
        onError("pdf-lib not loaded. Add pdf-lib script to index.html")
      case Some(lib) =>
        dom.console.log("[PdfLib] fetching PDF...")
        val result = for
          response <- dom.fetch(url).toFuture
          _        = dom.console.log("[PdfLib] fetch response status: " + response.status)
          bytes   <- response.arrayBuffer().toFuture
          doc     <- lib.PDFDocument.load(bytes).asInstanceOf[js.Promise[js.Dynamic]].toFuture
        yield doc
        result.foreach(onLoaded)
        result.failed.foreach { t =>
          dom.console.error("[PdfLib] load failed:", t)
          onError(t.getMessage)
        }

  def getForm(doc: js.Dynamic): js.Dynamic =
    doc.getForm()

  def getFieldNames(form: js.Dynamic): scala.collection.Seq[String] =
    val fields = form.getFields().asInstanceOf[js.Array[js.Dynamic]]
    (0 until fields.length).map(i => fields(i).getName().asInstanceOf[String])

  def getTextField(form: js.Dynamic, name: String): js.Dynamic =
    form.getTextField(name)

  def getCheckBox(form: js.Dynamic, name: String): js.Dynamic =
    form.getCheckBox(name)

  def removeMaxLength(field: js.Dynamic): Unit =
    val _ = field.setMaxLength(js.undefined)

  def setFontSize(field: js.Dynamic, pt: Int): Unit =
    val _ = field.setFontSize(pt)

  def setText(field: js.Dynamic, value: String): Unit =
    val _ = field.setText(value)

  def check(checkbox: js.Dynamic): Unit =
    val _ = checkbox.check()

  def saveAndOpen(doc: js.Dynamic, filename: String): Unit =
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    dom.console.log("[PdfLib] saveAndOpen: calling doc.save()")
    val future = doc.save().asInstanceOf[js.Promise[js.Any]].toFuture
    future.failed.foreach { t =>
      dom.console.error("[PdfLib] doc.save() failed:", t)
    }
    future.foreach { pdfBytes =>
      dom.console.log("[PdfLib] save() done, creating blob and download")
      try
        val uint8 = pdfBytes.asInstanceOf[js.typedarray.Uint8Array]
        val blob = new dom.Blob(
          js.Array(uint8),
          new dom.BlobPropertyBag { `type` = "application/pdf" }
        )
        val url = dom.URL.createObjectURL(blob)
        triggerDownload(url, filename)
        dom.console.log("[PdfLib] download triggered for " + filename)
        // Revoke after a short delay so the browser has time to start the download
        dom.window.setTimeout(() => dom.URL.revokeObjectURL(url), 2000)
      catch
        case t: Throwable =>
          dom.console.error("[PdfLib] blob/download error:", t)
    }

  private def triggerDownload(url: String, filename: String): Unit =
    val link = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
    link.href = url
    link.setAttribute("download", filename)
    discard(dom.document.body.appendChild(link))
    link.click()
    discard(dom.document.body.removeChild(link))

  private def discard(value: Any): Unit = ()
