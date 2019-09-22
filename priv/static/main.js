const connection = new WebSocket('ws://localhost:8765/user/connect');

connection.onopen = () => {
  console.log('connected');
};

connection.onclose = () => {
  console.error('disconnected');
};

connection.onerror = (error) => {
  console.error('failed to connect', error);
};

connection.onmessage = (event) => {
  var Pbf = require('pbf');
  var msg = require('./msg.js').msg;
  var pbf = new Pbf(event.data);
  var obj = msg.read(pbf);
  console.log('Msg received:');
  console.log(Object.keys(obj));
  console.log(obj["from"]);
  console.log(obj["to"]);
  console.log(obj["id"]);
  console.log(obj["content"]);
  console.log(obj["message_type"]);
  let li = document.createElement('li');
  li.innerText = obj["content"];
  document.querySelector('#chat').append(li);
};

document.querySelector('form').addEventListener('submit', (event) => {
  event.preventDefault();
  let message = document.querySelector('#message').value;
  let from = document.querySelector('#from').value;
  let to = document.querySelector('#to').value;
  let msg_type = document.querySelector('#msg_type').value;
  
  var obj = {
    from:from,
    to:to,
    content:message,
    message_type:msg_type,
    id:"asdf",
    timestamp:"1234"
  };

  var Pbf = require('pbf');
  var msg = require('./msg.js').msg;

  var pbf = new Pbf();
  msg.write(obj, pbf);
  var buffer = pbf.finish();

  connection.send(buffer);
  document.querySelector('#message').value = generate_random_string(5);
  document.querySelector('#msg_type').value = "2";
});

function generate_random_string(string_length){
  let random_string = '';
  let random_ascii;
  for(let i = 0; i < string_length; i++) {
      random_ascii = Math.floor((Math.random() * 25) + 97);
      random_string += String.fromCharCode(random_ascii)
  }
  return random_string
}