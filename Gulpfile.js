
'use strict';

const { series, parallel } = require('gulp');

const fs = require("fs");
const axios = require("axios");
const spinner = require("ora-promise")
const md5 = require("md5");
const $ = require("shelljs");

/* --------------------------------------------------------------- */

const VALUESET = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/value-sets.csv.gpg",
  "MD5": "e84c5c41ab44f34bf6c714ffe35d3669",
  "LOC": "./data/value-set.csv.gpg",
  "DES": "valueset crosswalk"
};

const TRIMESTER = {
  "URL": "https://mlua.s3.us-east-2.amazonaws.com/trimester-data.csv.gpg",
  "MD5": "35136438a780fd1235c6a36f6422c7b1",
  "LOC": "./data/trimester.csv.gpg",
  "DES": "trimester data"
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

const downloadTrimesterData = (cb) => {
  if (fs.existsSync(TRIMESTER.LOC)){
    console.log(`already have ${TRIMESTER.DES}`);
    return cb();
  }
  return spinner(`downloading ${TRIMESTER.DES}`,
    () => axios.get(TRIMESTER.URL)).
    then(resp => fs.promises.writeFile(TRIMESTER.LOC, resp.data)).
    catch(err => console.error(`failure: ${err}`));
};

const downloadValuesetData = (cb) => {
  if (fs.existsSync(VALUESET.LOC)){
    console.log(`already have ${VALUESET.DES}`);
    return cb();
  }
  return spinner(`downloading ${VALUESET.DES}`,
    () => axios.get(VALUESET.URL)).
    then(resp => fs.promises.writeFile(VALUESET.LOC, resp.data)).
    catch(err => console.error(`failure: ${err}`));
};

/* ---------------------------------------------------------------
 *
 * This are the targets that download the data sources
 * and place them in `./data`
 *
 */

const checkTrimesterData = (cb) => {
  return fs.promises.readFile(TRIMESTER.LOC).
    then(buf => {
      if (md5(buf) !== TRIMESTER.MD5){
        throw Error(`Unexpected change in ${TRIMESTER.LOC}`);
      } else {
        console.log("Hash of Trimester data is as expected");
      }
    });
};

// TODO make DRY-er
const checkValuesetXwalk = (cb) => {
  return fs.promises.readFile(VALUESET.LOC).
    then(buf => {
      if (md5(buf) !== VALUESET.MD5){
        throw Error(`Unexpected change in ${VALUESET.LOC}`);
      } else {
        console.log("Valueset crosswalk data is as expected");
      }
    });
};



/* ---------------------------------------------------------------
 *
 * This are the targets that download the data sources
 * and place them in `./data`
 *
 */

// const analyzeDogData = (cb) => {
//   $.exec("Rscript ./analyze-dog-data.R");
//   cb();
// };


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
exports.download  = series(downloadTrimesterData, downloadValuesetData);
exports.check     = parallel(checkTrimesterData, checkValuesetXwalk);
// exports.analyze   = analyzeDogData;

exports.default   = series(exports.setup,
                           exports.download,
                           exports.check);
                           // exports.analyze);

