#!/usr/bin/env python3
"""
LLM Helper Script for Goal-Pattern
Supports ChatGPT and Gemini APIs
"""

import argparse
import json
import os
import sys
import time
from typing import Dict, Any, List

# Try to import OpenAI and Google AI libraries
try:
    import openai
    OPENAI_AVAILABLE = True
except ImportError:
    OPENAI_AVAILABLE = False

try:
    import google.generativeai as genai
    GENAI_AVAILABLE = True
except ImportError:
    GENAI_AVAILABLE = False


class LLMHelper:
    """Helper class for LLM API interactions"""
    
    def __init__(self, provider: str = 'gemini'):
        self.provider = provider.lower()
        self.api_key = None
        self._load_api_key()
    
    def _load_api_key(self):
        """Load API key from environment or key file"""
        if self.provider == 'gemini':
            self.api_key = os.getenv('GEMINI_API_KEY')
            if not self.api_key and os.path.exists('chatgpt_qa_key.pl'):
                # Try to extract from Prolog key file
                try:
                    with open('chatgpt_qa_key.pl', 'r') as f:
                        content = f.read()
                        # Extract key from chatgpt_key("...").
                        import re
                        match = re.search(r'chatgpt_key\("([^"]*)"\)', content)
                        if match:
                            self.api_key = match.group(1)
                except Exception as e:
                    print(f"Warning: Could not read key file: {e}", file=sys.stderr)
        elif self.provider == 'chatgpt':
            self.api_key = os.getenv('OPENAI_API_KEY')
        
        if not self.api_key or self.api_key == "":
            print(f"Warning: No API key found for {self.provider}", file=sys.stderr)
    
    def call_llm(self, input_pack: Dict[str, Any]) -> Dict[str, Any]:
        """Call LLM provider with input pack and return response"""
        if self.provider == 'gemini':
            return self._call_gemini(input_pack)
        elif self.provider == 'chatgpt':
            return self._call_chatgpt(input_pack)
        else:
            raise ValueError(f"Unsupported provider: {self.provider}")
    
    def _call_gemini(self, input_pack: Dict[str, Any]) -> Dict[str, Any]:
        """Call Google Gemini API"""
        if not GENAI_AVAILABLE:
            return self._mock_response(input_pack, "Gemini library not installed")
        
        if not self.api_key or self.api_key == "":
            return self._mock_response(input_pack, "No Gemini API key")
        
        try:
            genai.configure(api_key=self.api_key)
            
            # Use Gemini 2.0 Flash model
            model = genai.GenerativeModel('gemini-2.0-flash-exp')
            
            # Build prompt from input pack
            system_prompt = input_pack.get('system_prompt', '')
            user_prompt = input_pack.get('user_prompt', '')
            
            full_prompt = f"{system_prompt}\n\n{user_prompt}"
            
            # Call API
            response = model.generate_content(full_prompt)
            
            # Parse response
            suggestions = self._parse_gemini_response(response.text, input_pack)
            
            return {
                'provider': 'gemini',
                'model': 'gemini-2.0-flash-exp',
                'suggestions': suggestions,
                'confidence': 0.75,
                'response_id': str(time.time()),
                'raw_response': response.text
            }
        
        except Exception as e:
            print(f"Error calling Gemini: {e}", file=sys.stderr)
            return self._mock_response(input_pack, f"Gemini error: {str(e)}")
    
    def _call_chatgpt(self, input_pack: Dict[str, Any]) -> Dict[str, Any]:
        """Call OpenAI ChatGPT API"""
        if not OPENAI_AVAILABLE:
            return self._mock_response(input_pack, "OpenAI library not installed")
        
        if not self.api_key:
            return self._mock_response(input_pack, "No OpenAI API key")
        
        try:
            openai.api_key = self.api_key
            
            # Build messages
            system_prompt = input_pack.get('system_prompt', '')
            user_prompt = input_pack.get('user_prompt', '')
            
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt}
            ]
            
            # Call API
            response = openai.ChatCompletion.create(
                model="gpt-4",
                messages=messages,
                temperature=0.7,
                max_tokens=2000
            )
            
            # Parse response
            content = response.choices[0].message.content
            suggestions = self._parse_chatgpt_response(content, input_pack)
            
            return {
                'provider': 'chatgpt',
                'model': 'gpt-4',
                'suggestions': suggestions,
                'confidence': 0.75,
                'response_id': response.id,
                'raw_response': content
            }
        
        except Exception as e:
            print(f"Error calling ChatGPT: {e}", file=sys.stderr)
            return self._mock_response(input_pack, f"ChatGPT error: {str(e)}")
    
    def _parse_gemini_response(self, text: str, input_pack: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Parse Gemini response text into suggestions"""
        # Simple parsing: treat whole response as one suggestion
        # In production, you might parse structured output
        
        context = input_pack.get('context', {})
        work_type = context.get('type', 'unknown')
        
        # Estimate counts
        word_count = len(text.split())
        clause_count = text.count('.') if work_type == 'algorithm' else 0
        
        return [{
            'text': text,
            'type': 'completion',
            'confidence': 0.75,
            'citations': [],
            'word_count': word_count,
            'clause_count': clause_count
        }]
    
    def _parse_chatgpt_response(self, text: str, input_pack: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Parse ChatGPT response text into suggestions"""
        # Similar to Gemini parsing
        context = input_pack.get('context', {})
        work_type = context.get('type', 'unknown')
        
        word_count = len(text.split())
        clause_count = text.count('.') if work_type == 'algorithm' else 0
        
        return [{
            'text': text,
            'type': 'completion',
            'confidence': 0.75,
            'citations': [],
            'word_count': word_count,
            'clause_count': clause_count
        }]
    
    def _mock_response(self, input_pack: Dict[str, Any], reason: str) -> Dict[str, Any]:
        """Return a mock response for testing"""
        return {
            'provider': self.provider,
            'model': 'mock',
            'suggestions': [{
                'text': f'Mock suggestion: {reason}',
                'type': 'completion',
                'confidence': 0.5,
                'citations': [],
                'word_count': 10,
                'clause_count': 2
            }],
            'confidence': 0.5,
            'response_id': 'mock_' + str(time.time()),
            'raw_response': f'Mock response - {reason}'
        }


def main():
    parser = argparse.ArgumentParser(description='LLM Helper for Goal-Pattern')
    parser.add_argument('--provider', default='gemini', choices=['gemini', 'chatgpt'],
                       help='LLM provider to use')
    parser.add_argument('--input', required=True, help='Path to input JSON file')
    
    args = parser.parse_args()
    
    # Read input
    try:
        with open(args.input, 'r') as f:
            input_pack = json.load(f)
    except Exception as e:
        print(json.dumps({
            'error': f'Failed to read input: {str(e)}',
            'suggestions': []
        }))
        sys.exit(1)
    
    # Call LLM
    helper = LLMHelper(provider=args.provider)
    response = helper.call_llm(input_pack)
    
    # Output response
    print(json.dumps(response, indent=2))
    sys.exit(0)


if __name__ == '__main__':
    main()
