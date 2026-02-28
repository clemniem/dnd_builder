package dndbuilder.common.pdf

import org.scalajs.dom

import scala.scalajs.js

object PdfLib:

  private def pdfLib: Option[js.Dynamic] =
    val lib = js.Dynamic.global.selectDynamic("PDFLib")
    if js.typeOf(lib) == "undefined" then None else Some(lib)

  def loadFromUrl(url: String)(onLoaded: js.Dynamic => Unit)(onError: String => Unit): Unit =
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    pdfLib match
      case None =>
        onError("pdf-lib not loaded. Add pdf-lib script to index.html")
      case Some(lib) =>
        val result = for
          response <- dom.fetch(url).toFuture
          bytes    <- response.arrayBuffer().toFuture
          doc      <- lib.PDFDocument.load(bytes).asInstanceOf[js.Promise[js.Dynamic]].toFuture
        yield doc
        result.foreach(onLoaded)
        result.failed.foreach(t => onError(t.getMessage))

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
    doc.save().asInstanceOf[js.Promise[js.Any]].toFuture.foreach { pdfBytes =>
      val uint8 = pdfBytes.asInstanceOf[js.typedarray.Uint8Array]
      val blob = new dom.Blob(
        js.Array(uint8),
        new dom.BlobPropertyBag { `type` = "application/pdf" }
      )
      val url = dom.URL.createObjectURL(blob)
      triggerDownload(url, filename)
      dom.URL.revokeObjectURL(url)
    }

  private def triggerDownload(url: String, filename: String): Unit =
    val link = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
    link.href = url
    link.setAttribute("download", filename)
    discard(dom.document.body.appendChild(link))
    link.click()
    discard(dom.document.body.removeChild(link))

  private def discard(value: Any): Unit = ()
