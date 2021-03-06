#!/usr/bin/env python
import asyncio
import websockets
import proto.msg_pb2
import sys
from datetime import datetime
import time
import ast

Uri = "ws://localhost:8765/user/connect"
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
        
async def profile_n_messages(n):
    AuthMsg = make_msg("Alek", "server", "dummy", 1)
    BinAuthMsg = AuthMsg.SerializeToString()
    RealMsg = make_msg("Alek", "Alek", "Hi", 2)
    BinRealMsg = RealMsg.SerializeToString()
    async with websockets.connect(Uri) as websocket:
        await websocket.send(BinAuthMsg)
        await websocket.recv()
        # BinAuthRes = await websocket.recv()
        # AuthRes = parse_and_print(BinAuthRes)
        # print(AuthRes)
        start = time.time()
        for _ in range(n):
            await websocket.send(BinRealMsg)
            await websocket.recv() # HTTP_RESPONSE
            await websocket.recv() # MESSAGE
        end = time.time()
        # BinSendConfirm = await websocket.recv()
        # BinRecievieMsg = await websocket.recv()
        # parse_and_print(BinSendConfirm)
        # parse_and_print(BinRecievieMsg)
        exec_time = end - start
        print(f"Execution time for all {n} messages is {exec_time} seconds")
        print(f"Execution time for 1 message is {exec_time / n} seconds")


if len(sys.argv) == 1:
    asyncio.get_event_loop().run_until_complete(profile_n_messages(1))
else:
    n = int(ast.literal_eval(str(sys.argv))[1])
    asyncio.get_event_loop().run_until_complete(profile_n_messages(n))