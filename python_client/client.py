#!/usr/bin/env python
import asyncio
import websockets
import proto.msg_pb2
import sys
from datetime import datetime
import time

Uri = "ws://localhost:8765/user/connect"
Login = ""
Token = "dummmy token"

def make_msg(From, To, Token, Type):
    auth_msg = proto.msg_pb2.msg()
    setattr(auth_msg, 'from', From)
    auth_msg.to = To
    auth_msg.content = Token
    auth_msg.id = From + " to " + To
    auth_msg.timestamp = str(datetime.now())
    auth_msg.message_type = Type # 0 = HTTP_RESPONSE, 1 = AUTH_TOKEN, MESSAGE = 2
    return auth_msg

def parse_and_print(Data):
    Res = proto.msg_pb2.msg()
    Res.ParseFromString(Data)
    print(Res)
    return Res
        
AuthMsg = make_msg("Alek", "server", "dummy", 1)
BinAuthMsg = AuthMsg.SerializeToString()
# print(AuthMsg)
RealMsg = make_msg("Alek", "Alek", "Hi", 2)
BinRealMsg = RealMsg.SerializeToString()

async def auth():
    async with websockets.connect(Uri) as websocket:
        await websocket.send(BinAuthMsg)
        BinAuthRes = await websocket.recv()
        AuthRes = parse_and_print(BinAuthRes)
        print(AuthRes)
        start = time.time()
        await websocket.send(BinRealMsg)
        BinSendConfirm = await websocket.recv()
        BinRecievieMsg = await websocket.recv()
        end = time.time()
        parse_and_print(BinSendConfirm)
        parse_and_print(BinRecievieMsg)
        print(f"Execution time is {end - start} seconds")

asyncio.get_event_loop().run_until_complete(auth())