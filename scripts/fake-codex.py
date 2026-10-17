#!/usr/bin/env python3
"""Deterministic Codex CLI and app-server peer for Cadre's smoke suite."""

import json
import subprocess
import sys


def send(message):
    print(json.dumps(message, separators=(",", ":")), flush=True)


def read_message():
    line = sys.stdin.readline()
    if not line:
        return None
    return json.loads(line)


def respond(message, result):
    send({"jsonrpc": "2.0", "id": message["id"], "result": result})


def main():
    args = sys.argv[1:]
    if args == ["--version"]:
        print("codex-cli 0.0.0-fake")
        return
    if args == ["login", "status"]:
        print("Logged in")
        return
    if args[:1] != ["app-server"]:
        print("unsupported fake codex command", file=sys.stderr)
        raise SystemExit(2)

    thread_id = "fake-codex-thread"
    turn_number = 0
    while True:
        message = read_message()
        if message is None:
            return
        method = message.get("method")
        if method == "initialize":
            respond(message, {"protocolVersion": "2025-06-18", "capabilities": {}})
        elif method == "initialized":
            continue
        elif method == "test/hang":
            # Used by the Lisp unit test to prove a missing response times out.
            continue
        elif method == "test/unicode":
            respond(message, message.get("params", {}).get("text", ""))
        elif method == "test/exit-with-inherited-stderr":
            # Keep only stderr inherited after this peer exits.
            child = subprocess.Popen(
                [sys.executable, "-c", "import time; time.sleep(10)"],
                stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL,
            )
            respond(message, child.pid)
            return
        elif method == "model/list":
            respond(
                message,
                {
                    "data": [
                        {
                            "id": "fake-gpt-default",
                            "model": "fake-gpt-default",
                            "displayName": "Fake GPT Default",
                            "description": "Fake model for smoke tests",
                            "hidden": False,
                            "isDefault": True,
                            "defaultReasoningEffort": "medium",
                            "supportedReasoningEfforts": [
                                {"reasoningEffort": "medium", "description": "Balanced"}
                            ],
                        },
                        {"id": "fake-hidden", "hidden": True},
                    ]
                },
            )
        elif method == "thread/start":
            respond(message, {"thread": {"id": thread_id}})
        elif method == "thread/resume":
            thread_id = message["params"]["threadId"]
            respond(message, {"thread": {"id": thread_id}})
        elif method == "turn/start":
            params = message.get("params", {})
            turn_number += 1
            turn_id = f"fake-codex-turn-{turn_number}"
            respond(message, {"turn": {"id": turn_id, "status": "inProgress"}})
            send(
                {
                    "jsonrpc": "2.0",
                    "method": "turn/started",
                    "params": {"threadId": thread_id, "turn": {"id": turn_id}},
                }
            )

            prompt = "\n".join(item.get("text", "") for item in params.get("input", []))
            if "trigger fake failure" in prompt:
                send(
                    {
                        "jsonrpc": "2.0",
                        "method": "turn/completed",
                        "params": {
                            "threadId": thread_id,
                            "turn": {
                                "id": turn_id,
                                "status": "failed",
                                "error": {"message": "Fake Codex turn failure"},
                            },
                        },
                    }
                )
                continue

            # Exercise app-server's server-initiated dynamic tool path.
            send(
                {
                    "jsonrpc": "2.0",
                    "id": 900,
                    "method": "item/tool/call",
                    "params": {
                        "threadId": thread_id,
                        "turnId": turn_id,
                        "callId": "fake-context-call",
                        "tool": "current_context",
                        "arguments": {},
                    },
                }
            )
            tool_reply = read_message()
            if tool_reply is None or tool_reply.get("id") != 900:
                return

            if "retry after fake failure" in prompt:
                reply = "Fake Codex recovered after failure"
            elif "You are in agent mode" in prompt:
                reply = "Fake Codex agent mode"
            else:
                reply = "Fake **Codex** reply"
            send(
                {
                    "jsonrpc": "2.0",
                    "method": "item/agentMessage/delta",
                    "params": {"threadId": thread_id, "turnId": turn_id, "delta": reply},
                }
            )
            send(
                {
                    "jsonrpc": "2.0",
                    "method": "turn/completed",
                    "params": {
                        "threadId": thread_id,
                        "turn": {"id": turn_id, "status": "completed"},
                    },
                }
            )
        elif method == "turn/interrupt":
            respond(message, {})
        else:
            if "id" in message:
                send(
                    {
                        "jsonrpc": "2.0",
                        "id": message["id"],
                        "error": {"code": -32601, "message": f"Unknown fake method: {method}"},
                    }
                )


if __name__ == "__main__":
    main()
