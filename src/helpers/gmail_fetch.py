#!/usr/bin/env python3
"""
Gmail Sent Mail Fetcher
Authenticates with Gmail API and fetches sent emails to a specified address.
Outputs metadata as JSON for Prolog consumption.
"""

import json
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

# Try to import Google API libraries
try:
    from google.auth.transport.requests import Request
    from google.oauth2.credentials import Credentials
    from google_auth_oauthlib.flow import InstalledAppFlow
    from googleapiclient.discovery import build
    from googleapiclient.errors import HttpError
    GOOGLE_LIBS_AVAILABLE = True
except ImportError:
    GOOGLE_LIBS_AVAILABLE = False

# Gmail API scope - read-only access to sent messages
SCOPES = ['https://www.googleapis.com/auth/gmail.readonly']

def get_credentials(credentials_path='config/gmail_credentials.json', 
                    token_path='config/gmail_token.json'):
    """
    Get Gmail API credentials, prompting for authentication if needed.
    Credentials are stored securely outside the repository.
    """
    creds = None
    
    # Check for existing token
    if os.path.exists(token_path):
        try:
            creds = Credentials.from_authorized_user_file(token_path, SCOPES)
        except Exception as e:
            print(f"Warning: Failed to load token: {e}", file=sys.stderr)
    
    # If no valid credentials available, authenticate
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            try:
                creds.refresh(Request())
            except Exception as e:
                print(f"Warning: Failed to refresh token: {e}", file=sys.stderr)
                creds = None
        
        if not creds:
            if not os.path.exists(credentials_path):
                print(f"Error: Credentials file not found at {credentials_path}", file=sys.stderr)
                print("Please download OAuth 2.0 credentials from Google Cloud Console", file=sys.stderr)
                sys.exit(1)
            
            flow = InstalledAppFlow.from_client_secrets_file(credentials_path, SCOPES)
            creds = flow.run_local_server(port=0)
        
        # Save credentials for future use
        with open(token_path, 'w') as token:
            token.write(creds.to_json())
    
    return creds

def fetch_sent_emails(to_address=None, days_back=7, max_results=100):
    """
    Fetch sent emails from Gmail.
    
    Args:
        to_address: Filter by recipient address (optional)
        days_back: How many days back to fetch (default: 7)
        max_results: Maximum number of messages to fetch (default: 100)
    
    Returns:
        List of email metadata dictionaries
    """
    if not GOOGLE_LIBS_AVAILABLE:
        print("Error: Google API libraries not installed", file=sys.stderr)
        print("Install with: pip install google-auth google-auth-oauthlib google-auth-httplib2 google-api-python-client", file=sys.stderr)
        sys.exit(1)
    
    try:
        creds = get_credentials()
        service = build('gmail', 'v1', credentials=creds)
        
        # Build query
        query_parts = ['in:sent']
        if to_address:
            query_parts.append(f'to:{to_address}')
        
        # Date filter
        after_date = datetime.now() - timedelta(days=days_back)
        after_str = after_date.strftime('%Y/%m/%d')
        query_parts.append(f'after:{after_str}')
        
        query = ' '.join(query_parts)
        
        # Fetch messages
        results = service.users().messages().list(
            userId='me',
            q=query,
            maxResults=max_results
        ).execute()
        
        messages = results.get('messages', [])
        
        # Fetch full metadata for each message
        email_metadata = []
        for msg in messages:
            msg_id = msg['id']
            msg_data = service.users().messages().get(
                userId='me',
                id=msg_id,
                format='metadata',
                metadataHeaders=['Subject', 'To', 'Date']
            ).execute()
            
            # Extract headers
            headers = {h['name']: h['value'] for h in msg_data.get('payload', {}).get('headers', [])}
            
            # Get snippet (preview text)
            snippet = msg_data.get('snippet', '')
            
            # Get thread ID
            thread_id = msg_data.get('threadId', '')
            
            # Get internal date (milliseconds since epoch)
            internal_date_ms = int(msg_data.get('internalDate', 0))
            internal_date = internal_date_ms / 1000.0  # Convert to seconds
            
            email_metadata.append({
                'message_id': msg_id,
                'thread_id': thread_id,
                'subject': headers.get('Subject', ''),
                'to': headers.get('To', ''),
                'date': headers.get('Date', ''),
                'timestamp': internal_date,
                'snippet': snippet[:500] if snippet else ''  # Limit snippet length
            })
        
        return email_metadata
    
    except HttpError as error:
        print(f'An error occurred: {error}', file=sys.stderr)
        sys.exit(1)
    except Exception as error:
        print(f'Unexpected error: {error}', file=sys.stderr)
        sys.exit(1)

def main():
    """
    Main entry point for the script.
    Parses command line arguments and outputs JSON.
    """
    import argparse
    
    parser = argparse.ArgumentParser(description='Fetch Gmail sent mail metadata')
    parser.add_argument('--to', help='Filter by recipient email address', default=None)
    parser.add_argument('--days', type=int, help='Number of days back to fetch', default=7)
    parser.add_argument('--max', type=int, help='Maximum number of messages', default=100)
    parser.add_argument('--output', help='Output JSON file path', default=None)
    parser.add_argument('--dry-run', action='store_true', help='Dry run mode (no actual API calls)')
    
    args = parser.parse_args()
    
    if args.dry_run:
        # Return sample data for testing
        sample_data = [
            {
                'message_id': 'sample_msg_1',
                'thread_id': 'sample_thread_1',
                'subject': 'Completed 15 algorithms this week',
                'to': 'example@example.com',
                'date': 'Mon, 30 Dec 2024 10:00:00 +0000',
                'timestamp': 1735552800.0,
                'snippet': 'I completed 15 algorithms this week including new predicates for goal tracking.'
            },
            {
                'message_id': 'sample_msg_2',
                'thread_id': 'sample_thread_2',
                'subject': 'Submitted philosophy essay',
                'to': 'example@example.com',
                'date': 'Mon, 30 Dec 2024 14:00:00 +0000',
                'timestamp': 1735567200.0,
                'snippet': 'Just submitted my philosophy essay on epistemic logic.'
            }
        ]
        result = {'emails': sample_data, 'count': len(sample_data)}
    else:
        emails = fetch_sent_emails(
            to_address=args.to,
            days_back=args.days,
            max_results=args.max
        )
        result = {'emails': emails, 'count': len(emails)}
    
    # Output JSON
    if args.output:
        with open(args.output, 'w') as f:
            json.dump(result, f, indent=2)
        print(f"Fetched {result['count']} emails, saved to {args.output}", file=sys.stderr)
    else:
        print(json.dumps(result, indent=2))
    
    return 0

if __name__ == '__main__':
    sys.exit(main())
