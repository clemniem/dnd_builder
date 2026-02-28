// Optional module: pdf — uncomment the next two lines if PDF module is enabled
// import { jsPDF } from "jspdf";
// window.jspdf = { jsPDF };

import { TyrianApp, testPdf } from "./target/scalajs-dev/main.js";

window.testPdf = testPdf;

TyrianApp.launch("myapp");
