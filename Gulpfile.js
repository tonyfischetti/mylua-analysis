
'use strict';

const { series, parallel } = require('gulp');

const fs = require("fs");
const axios = require("axios");
const spinner = require("ora-promise")
const md5 = require("md5");
const $ = require("shelljs");
const pgp = require("openpgp");
const path = require("path");

const lua = require("./lua-utils");

require('dotenv').config();

/* --------------------------------------------------------------- */

const INPUT_DATA = {};

// INPUT_DATA.TRIMESTER = {
//   "URL": "https://mlua.s3.us-east-2.amazonaws.com/trimester-data.csv.gpg",
//   "MD5": "3cf24f02a41daffa4e18532c98815676",
//   "LOC": "./data/trimester.csv.gpg",
//   "DES": "trimester data"
// };

INPUT_DATA.TRIMESTER = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/trimester.csv.asc",
  "MD5": "d111e6db9a8c27a789afafdcbf2731c3",
  "LOC": "./data/trimester.csv.asc",
  "DES": "trimester data"
};

INPUT_DATA.VALUESET = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/valuesets.csv.asc",
  "MD5": "2af193be72bfa814ef929566a4e24832",
  "LOC": "./data/valuesets.csv.asc",
  "DES": "valueset crosswalk"
};

INPUT_DATA.DELIVERY_DATA = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/delivery.csv.asc",
  "MD5": "ab3dc716c4b3b698ef3a06100e3ff791",
  "LOC": "./data/delivery.csv.asc",
  "DES": "FILL OUT LATER"
};

INPUT_DATA.DEMO_DATA = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/demo.csv.asc",
  "MD5": "08f3a1e83c3aff9209e7b5d813d688f2",
  "LOC": "./data/demo.csv.asc",
  "DES": "FILL OUT LATER"
};

INPUT_DATA.OBS_XWALK = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/obs-xwalk.csv.asc",
  "MD5": "a4f54c0cfb979963c95da642f03a46af",
  "LOC": "./data/obs-xwalk.csv.asc",
  "DES": "FILL OUT LATER"
};

INPUT_DATA.SCREENING_DATA = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/screening.csv.asc",
  "MD5": "ab0b9c9c02a4de269fffc199bcf9a280",
  "LOC": "./data/screening.csv.asc",
  "DES": "FILL OUT LATER!"
};

/* --------------------------------------------------------------- */


/* ---------------------------------------------------------------
 *
 * This is the target that creates the `data` and `target`
 * directories.
 *
 */

const setupDirs = (cb) => {
  $.mkdir("-p", "data");
  $.mkdir("-p", "target");
  return cb();
};


/* ---------------------------------------------------------------
 *
 * These are the targets that download the data sources
 * and place them in './data'
 *
 */

const downloadADatum = (cb, dataObj) => {
  if (fs.existsSync(dataObj.LOC)) {
    console.log(`already have ${dataObj.DES}`);
    return cb();
  }
  return spinner(`downloading ${dataObj.DES}`,
    () => axios.get(dataObj.URL)).
    then(resp => fs.promises.writeFile(dataObj.LOC, resp.data)).
    catch(err => console.error(`failure: ${err}`));
};

const downloadTrimesterData = (cb)  => downloadADatum(cb, INPUT_DATA.TRIMESTER);
const downloadValuesetData = (cb)   => downloadADatum(cb, INPUT_DATA.VALUESET);
const downloadDeliveryData = (cb)   => downloadADatum(cb, INPUT_DATA.DELIVERY_DATA);
const downloadDemoData = (cb)       => downloadADatum(cb, INPUT_DATA.DEMO_DATA);
const downloadObsXWalk = (cb)       => downloadADatum(cb, INPUT_DATA.OBS_XWALK);
const downloadScreeningData = (cb)  => downloadADatum(cb, INPUT_DATA.SCREENING_DATA);


/* ---------------------------------------------------------------
 *
 * These are the targets that download the data sources
 * and place them in `./data`
 *
 */

const checkADatum = (cb, dataObj) => {
  return fs.promises.readFile(dataObj.LOC).
    then(buf => {
      if (md5(buf) !== dataObj.MD5){
        throw Error(`Unexpected change in ${dataObj.LOC}`);
      } else {
        console.log("Hash of Trimester data is as expected");
      }
    });
};

const checkTrimesterData = (cb)  => checkADatum(cb, INPUT_DATA.TRIMESTER);
const checkValuesetData = (cb)   => checkADatum(cb, INPUT_DATA.VALUESET);
const checkDeliveryData = (cb)   => checkADatum(cb, INPUT_DATA.DELIVERY_DATA);
const checkDemoData = (cb)       => checkADatum(cb, INPUT_DATA.DEMO_DATA);
const checkObsXWalk = (cb)       => checkADatum(cb, INPUT_DATA.OBS_XWALK);
const checkScreeningData = (cb)  => checkADatum(cb, INPUT_DATA.SCREENING_DATA);



/* ---------------------------------------------------------------
 *
 * This is the target that decrypts the files
 *
 */

const decryptFiles = (cb) => {
  Promise.resolve().
    then(() => lua.decryptFile("./data/valuesets.csv.asc", process.env.GPGPASS)).
    then(() => lua.decryptFile("./data/trimester.csv.asc",  process.env.GPGPASS)).
    then(() => lua.decryptFile("./data/demo.csv.asc",  process.env.GPGPASS)).
    then(() => cb());
};


/* ---------------------------------------------------------------
 *
 * This are the targets that download the data sources
 * and place them in `./data`
 *
 */

const doFeatureEngineering = (cb) => {
  $.exec("R_LIBS='~/local/R_libs' Rscript feature-engineering.R");
  cb();
};


/* ---------------------------------------------------------------
 *
 * Finally, this is a target that cleans the generated directories
 * and place them in `./data`
 *
 * It's not in the default pipeline because I don't really
 * know what the security ramifications are of force removing
 * a directory.
 *
 */

const mrproper = (cb) => {
  $.rm("-rf", "data")
  $.rm("-rf", "target")
  cb();
};

/* --------------------------------------------------------------- */


/*
 * the "download" target doesn't have to be "series" (as opposed
 * to "parallel") but there's a race condition in TTY output if
 * it's not "series"
 */

exports.clean     = mrproper;
exports.setup     = setupDirs;

exports.download  = series(exports.setup,
                           parallel(downloadTrimesterData,
                                    downloadValuesetData,
                                    downloadDeliveryData,
                                    downloadDemoData,
                                    downloadObsXWalk,
                                    downloadScreeningData));

exports.check  = parallel(checkTrimesterData,
                          checkValuesetData,
                          checkDeliveryData,
                          checkDemoData,
                          checkObsXWalk,
                          checkScreeningData);

exports.decrypt   = decryptFiles;
exports.analyze   = doFeatureEngineering;

exports.default   = series(exports.setup,
                           exports.download,
                           exports.check,
                           exports.decrypt,
                           exports.analyze);

