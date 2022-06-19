
const pgp = require("openpgp");
const fs = require("fs");
const path = require("path");


exports.encryptFile = (filename, password) => {
  const resInFilename  = path.resolve(filename);
  const resOutFilename = path.resolve(`${resInFilename}.asc`);

  return fs.promises.readFile(resInFilename).
    then(file => new Uint8Array(file)).
    then(file2 => pgp.createMessage( { binary: file2 })).
    then(mes => pgp.encrypt({message: mes, passwords: [password], format: 'armored'})).
    then(enced => fs.promises.writeFile(resOutFilename, enced));
};

exports.decryptFile = async (filename, password) => {
  const resInFilename  = path.resolve(filename);
  const resOutFilename = path.resolve(path.dirname(resInFilename),
                                      path.basename(resInFilename, ".asc"));
  return fs.promises.readFile(resInFilename, 'utf-8').
    then(data => pgp.readMessage({ armoredMessage: data })).
    then(mes => pgp.decrypt({message: mes, passwords: [password], format: 'utf-8'})).
    then(cleartext => fs.promises.writeFile(resOutFilename, cleartext.data));
};



