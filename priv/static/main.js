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
  var pbf = new Pbf(event.data);
  var obj = Example.read(pbf);
  console.log('received', obj.content);
  let li = document.createElement('li');
  li.innerText = obj.content;
  document.querySelector('#chat').append(li);
};

document.querySelector('form').addEventListener('submit', (event) => {
  event.preventDefault();
  let message = document.querySelector('#message').value;
  let from = document.querySelector('#from').value;
  let to = document.querySelector('#to').value;
  let msg_type = document.querySelector('#msg_type').value;
  
  console.log(from);
  console.log(to);
  console.log(message);
  console.log(msg_type);
  
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
  document.querySelector('#message').value = '';
});