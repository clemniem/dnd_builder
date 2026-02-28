// Optional module: pdf — uncomment the next two lines if PDF module is enabled
// import { jsPDF } from "jspdf";
// window.jspdf = { jsPDF };

import { TyrianApp, testPdf, listPdfFields } from "./target/scalajs-dev/main.js";

window.testPdf = testPdf;
window.listPdfFields = listPdfFields;

TyrianApp.launch("myapp");
