
'use strict';

const { series, parallel } = require('gulp');

const fs = require("fs");
const axios = require("axios");
const spinner = require("ora-promise")
const md5 = require("md5");
const $ = require("shelljs");

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
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/trimester-v2-added.csv.gpg",
  "MD5": "c5765b3d58e04577f675ab3e4a6e1ad2",
  "LOC": "./data/trimester.csv.gpg",
  "DES": "trimester data"
};

INPUT_DATA.VALUESET = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/valuesets.csv.gpg",
  "MD5": "671409cb1171fa18f25df5f1c4ea4fed",
  "LOC": "./data/valuesets.csv.gpg",
  "DES": "valueset crosswalk"
};

INPUT_DATA.DELIVERY_DATA = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/delivery-data.csv.gpg",
  "MD5": "4960d82fa00da9ae3484fcb7217d4434",
  "LOC": "./data/delivery.csv.gpg",
  "DES": "FILL OUT LATER"
};

INPUT_DATA.DEMO_DATA = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/demo-data.csv.gpg",
  "MD5": "f20f9d93b7ec639a1b44e92d4604bee3",
  "LOC": "./data/demo.csv.gpg",
  "DES": "FILL OUT LATER"
};

INPUT_DATA.OBS_XWALK = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/obs-xwalk.csv.gpg",
  "MD5": "f7414dfff45b7146333ae728e7bc838c",
  "LOC": "./data/obs-walk.csv.gpg",
  "DES": "FILL OUT LATER"
};

INPUT_DATA.SCREENING_DATA = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/screening.csv.gpg",
  "MD5": "e6253eabae8828ebb9e69da91b7ee203",
  "LOC": "./data/screening.csv.gpg",
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
const downloadObsXwalk = (cb)       => downloadADatum(cb, INPUT_DATA.OBS_XWALK);
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
const checkObsXwalk = (cb)       => checkADatum(cb, INPUT_DATA.OBS_XWALK);
const checkScreeningData = (cb)  => checkADatum(cb, INPUT_DATA.SCREENING_DATA);



/* ---------------------------------------------------------------
 *
 * This is the target that decrypts the files
 *
 */

const decryptFiles = (cb) => {
  const all = $.ls("data/*.gpg");
  all.map(filename => {
    $.exec(`echo ${process.env.GPGPASS} | gpg --batch --yes --passphrase-fd 0 ${filename}`);
  });
  // $.exec(`echo ${process.env.GPGPASS} > hi`);
  return cb();
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

exports.download  = parallel(downloadTrimesterData,
                             downloadValuesetData,
                             downloadDeliveryData,
                             downloadDemoData,
                             downloadObsXwalk,
                             downloadScreeningData);

exports.check  = parallel(checkTrimesterData,
                          checkValuesetData,
                          checkDeliveryData,
                          checkDemoData,
                          checkObsXwalk,
                          checkScreeningData);

exports.decrypt   = decryptFiles;
exports.analyze   = doFeatureEngineering;

exports.default   = series(exports.setup,
                           exports.download,
                           exports.check,
                           exports.decrypt,
                           exports.analyze);

