% calendar_bridge.pl - Native calendar integration bridge (stub for Phase B)
:- module(calendar_bridge, [
    fetch_native_events/3,
    calendar_bridge_available/0
]).

:- use_module('../log').
:- use_module('../config').

% ============================================================================
% Phase B - Native Calendar Integration (macOS EventKit)
% ============================================================================

% This module provides a bridge interface for native calendar integration.
% Phase A uses ICS files (calendar_ics.pl).
% Phase B will integrate with macOS EventKit via an external helper.
%
% The external helper should:
% 1. Authenticate with the calendar system
% 2. Fetch events within a date range
% 3. Output JSON to a specified path
% 4. Return success/failure status

% ============================================================================
% Bridge Interface
% ============================================================================

% Check if native calendar bridge is available
calendar_bridge_available :-
    % Check if bridge helper exists
    (config:get_config([calendar, bridge_helper], Helper) ->
        (exists_file(Helper) ->
            log_info('Native calendar bridge available')
        ;
            log_debug('Native calendar bridge helper not found'),
            fail
        )
    ;
        log_debug('Native calendar bridge not configured'),
        fail
    ).

% Fetch events from native calendar system
% fetch_native_events(+Start, +End, -JsonPath)
% Start, End: Unix timestamps defining the date range
% JsonPath: Path where JSON output will be written
fetch_native_events(Start, End, JsonPath) :-
    calendar_bridge_available,
    config:get_config([calendar, bridge_helper], Helper),
    
    % Create command to fetch events
    format(atom(Command), '~w --start ~w --end ~w --output ~w', 
           [Helper, Start, End, JsonPath]),
    
    format(atom(Msg), 'Fetching native events: ~w to ~w', [Start, End]),
    log_info(Msg),
    
    % Execute bridge helper
    catch(
        (shell(Command, ExitCode),
         (ExitCode = 0 ->
             (format(atom(SuccessMsg), 'Native events fetched to: ~w', [JsonPath]),
              log_info(SuccessMsg))
         ;
             (format(atom(ErrorMsg), 'Bridge helper failed with code: ~w', [ExitCode]),
              log_error(ErrorMsg),
              fail)
         )),
        Error,
        (format(atom(ErrorMsg), 'Error executing bridge helper: ~w', [Error]),
         log_error(ErrorMsg),
         fail)
    ),
    !.

fetch_native_events(_Start, _End, _JsonPath) :-
    log_warn('Native calendar bridge not available, skipping'),
    fail.

% ============================================================================
% Future Implementation Notes
% ============================================================================

% When implementing Phase B, the bridge helper should output JSON in this format:
% {
%   "events": [
%     {
%       "uid": "unique-event-id",
%       "summary": "Event Title",
%       "location": "Location Name",
%       "start": 1234567890,  // Unix timestamp
%       "end": 1234567900,    // Unix timestamp
%       "status": "confirmed", // or "tentative", "cancelled"
%       "description": "Optional description",
%       "calendar": "Work",    // Calendar name
%       "attendees": [...],    // Optional attendee info
%       "recurrence": {...}    // Optional recurrence info
%     }
%   ]
% }
%
% The Prolog side will:
% 1. Read the JSON file
% 2. Parse events similar to ICS parsing
% 3. Apply the same tagging and attendance logic
% 4. Convert to schedule_event/... structures
% 5. Store in state with source = 'bridge'

% Example bridge helper implementations:
% - Swift using EventKit framework
% - Node.js using node-mac-calendar or similar
% - Python using PyObjC to access EventKit

% Security considerations:
% - Store calendar access credentials outside repo
% - Use system keychain or environment variables
% - Implement rate limiting
% - Respect user privacy preferences
% - Provide "offline mode" to disable bridge
