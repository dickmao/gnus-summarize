#!/usr/bin/env python3
import sys
import os
from pathlib import Path
from google import generativeai as genai

api_key = os.getenv('GEMINI_API_KEY')
if not api_key:
    print("ERROR: GEMINI_API_KEY not set", file=sys.stderr)
    sys.exit(1)

genai.configure(api_key=api_key)
model = genai.GenerativeModel('gemini-2.0-flash')
chat = model.start_chat(history=[])

if len(sys.argv) > 1:
    script_dir = Path(__file__).parent
    prompt = (script_dir / 'chat-prompt.txt').read_text()
    messages_file = Path(sys.argv[1])
    corpus = messages_file.read_text(encoding='utf-8', errors='replace')
    try:
        chat.send_message(prompt + "\n" + corpus)
    except Exception as e:
        print(f"Error loading corpus: {e}", file=sys.stderr)

print("Gemini> ", end='', flush=True)

for line in sys.stdin:
    user_input = line.strip()
    if not user_input:
        continue

    try:
        response = chat.send_message(user_input)
        print(response.text)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)

    print("\nGemini> ", end='', flush=True)
