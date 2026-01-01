% remind.pl - Reminder and notification engine
:- module(remind, [
    schedule_reminder/4,
    trigger_reminder/1,
    get_pending_reminders/1,
    get_due_reminders/2,
    cancel_reminder/1,
    clear_old_reminders/1,
    reminder_history/2,
    process_due_reminders/0,
    work_block_reminder_time/2,
    event_prep_reminder_time/2
]).

:- use_module(library(aggregate)).
:- use_module(state).
:- use_module(log).
:- use_module(preferences).

% Dynamic facts to store reminders
:- dynamic scheduled_reminder/4.
:- dynamic reminder_triggered/3.

% ============================================================================
% Reminder Types
% ============================================================================

% reminder(ID, Timestamp, Type, Context)
% Types: work_block, break, rest, event_prep, event_start, custom
% Context: additional information about the reminder

% ============================================================================
% Scheduling Reminders
% ============================================================================

% Schedule a reminder
% schedule_reminder(ID, Timestamp, Type, Context)
schedule_reminder(ID, Timestamp, Type, Context) :-
    atom(ID),
    number(Timestamp),
    atom(Type),
    get_time(Now),
    Timestamp > Now,  % Only schedule future reminders
    
    % Remove any existing reminder with same ID
    retractall(scheduled_reminder(ID, _, _, _)),
    
    % Add new reminder
    assertz(scheduled_reminder(ID, Timestamp, Type, Context)),
    
    format(atom(Msg), 'Reminder scheduled: ~w at ~w (~w)', [Type, Timestamp, ID]),
    log:log_info(Msg),
    !.

schedule_reminder(ID, Timestamp, Type, _) :-
    get_time(Now),
    (Timestamp =< Now ->
        format(atom(Msg), 'Cannot schedule reminder in the past: ~w', [ID]),
        log:log_warn(Msg)
    ;
        format(atom(Msg), 'Invalid reminder parameters: ~w, ~w, ~w', [ID, Timestamp, Type]),
        log:log_error(Msg)
    ),
    fail.

% ============================================================================
% Triggering Reminders
% ============================================================================

% Trigger a reminder (send notification)
trigger_reminder(ID) :-
    scheduled_reminder(ID, _, Type, Context),
    
    % Check if notifications are enabled
    preferences:get_preference(notifications_enabled, true),
    
    % Send notification
    send_notification(Type, Context),
    
    % Record that reminder was triggered
    get_time(Now),
    assertz(reminder_triggered(ID, Type, Now)),
    
    % Remove from scheduled reminders
    retractall(scheduled_reminder(ID, _, _, _)),
    
    format(atom(Msg), 'Reminder triggered: ~w (~w)', [Type, ID]),
    log:log_info(Msg),
    !.

trigger_reminder(ID) :-
    scheduled_reminder(ID, _, Type, _),
    
    % Notifications disabled, just log and remove
    format(atom(Msg), 'Reminder skipped (notifications disabled): ~w (~w)', [Type, ID]),
    log:log_info(Msg),
    retractall(scheduled_reminder(ID, _, _, _)),
    !.

trigger_reminder(ID) :-
    format(atom(Msg), 'Cannot trigger unknown reminder: ~w', [ID]),
    log:log_warn(Msg),
    fail.

% ============================================================================
% Notification Output
% ============================================================================

% Send notification to user (Terminal output initially)
send_notification(work_block, Context) :-
    (get_dict(block_type, Context, BlockType) -> true ; BlockType = work),
    (get_dict(duration, Context, Duration) -> true ; Duration = unknown),
    (get_dict(reason, Context, Reason) -> true ; Reason = 'Time to work'),
    
    format('~n=== REMINDER: Work Block ===~n', []),
    format('Type: ~w~n', [BlockType]),
    format('Duration: ~w minutes~n', [Duration]),
    format('Reason: ~w~n', [Reason]),
    format('===========================~n~n', []).

send_notification(break, Context) :-
    (get_dict(reason, Context, Reason) -> true ; Reason = 'Take a break'),
    
    format('~n=== REMINDER: Break Time ===~n', []),
    format('~w~n', [Reason]),
    format('============================~n~n', []).

send_notification(rest, Context) :-
    (get_dict(fatigue_level, Context, Fatigue) -> true ; Fatigue = unknown),
    
    format('~n=== REMINDER: Rest Needed ===~n', []),
    format('Fatigue level: ~w~n', [Fatigue]),
    format('Consider taking a longer rest or break.~n', []),
    format('=============================~n~n', []).

send_notification(event_prep, Context) :-
    (get_dict(event_title, Context, Title) -> true ; Title = 'Upcoming event'),
    get_dict(start_time, Context, StartTime),
    stamp_date_time(StartTime, DateTime, local),
    format_time(string(TimeStr), '%H:%M', DateTime),
    
    format('~n=== REMINDER: Event Preparation ===~n', []),
    format('Event: ~w~n', [Title]),
    format('Starts at: ~w~n', [TimeStr]),
    format('Prepare to leave soon.~n', []),
    format('===================================~n~n', []).

send_notification(event_start, Context) :-
    (get_dict(event_title, Context, Title) -> true ; Title = 'Event'),
    
    format('~n=== REMINDER: Event Starting ===~n', []),
    format('Event: ~w~n', [Title]),
    format('================================~n~n', []).

send_notification(custom, Context) :-
    (get_dict(message, Context, Message) -> true ; Message = 'Reminder'),
    
    format('~n=== REMINDER ===~n', []),
    format('~w~n', [Message]),
    format('================~n~n', []).

send_notification(Type, _) :-
    format('~n=== REMINDER ===~n', []),
    format('Type: ~w~n', [Type]),
    format('================~n~n', []).

% ============================================================================
% Reminder Queries
% ============================================================================

% Get all pending reminders
get_pending_reminders(Reminders) :-
    findall(reminder(ID, Timestamp, Type, Context),
            scheduled_reminder(ID, Timestamp, Type, Context),
            Reminders).

% Get reminders due before a given time
get_due_reminders(BeforeTime, DueReminders) :-
    findall(reminder(ID, Timestamp, Type, Context),
            (scheduled_reminder(ID, Timestamp, Type, Context),
             Timestamp =< BeforeTime),
            DueReminders).

% Cancel a scheduled reminder
cancel_reminder(ID) :-
    retractall(scheduled_reminder(ID, _, _, _)),
    format(atom(Msg), 'Reminder cancelled: ~w', [ID]),
    log:log_info(Msg).

% Clear old triggered reminders (older than given timestamp)
clear_old_reminders(BeforeTime) :-
    findall(ID,
            (reminder_triggered(ID, _, Time),
             Time < BeforeTime),
            OldIDs),
    maplist(retract_triggered_reminder, OldIDs),
    length(OldIDs, Count),
    format(atom(Msg), 'Cleared ~w old reminders', [Count]),
    log:log_info(Msg).

retract_triggered_reminder(ID) :-
    retractall(reminder_triggered(ID, _, _)).

% Get reminder history
reminder_history(AfterTime, History) :-
    findall(triggered(ID, Type, Time),
            (reminder_triggered(ID, Type, Time),
             Time >= AfterTime),
            History).

% ============================================================================
% Reminder Processing (called by daemon tick)
% ============================================================================

% Process all due reminders
process_due_reminders :-
    get_time(Now),
    get_due_reminders(Now, DueReminders),
    maplist(trigger_due_reminder, DueReminders),
    length(DueReminders, Count),
    (Count > 0 ->
        format(atom(Msg), 'Processed ~w due reminders', [Count]),
        log:log_info(Msg)
    ;
        true
    ).

trigger_due_reminder(reminder(ID, _, _, _)) :-
    trigger_reminder(ID).

% ============================================================================
% Utility Predicates
% ============================================================================

% Calculate reminder time for a work block (with lead time)
work_block_reminder_time(BlockStartTime, ReminderTime) :-
    preferences:get_preference(notification_lead_minutes, LeadMinutes),
    ReminderTime is BlockStartTime - (LeadMinutes * 60).

% Calculate reminder time for event preparation
event_prep_reminder_time(EventStartTime, ReminderTime) :-
    preferences:get_preference(travel_prep_buffer_minutes, PrepMinutes),
    ReminderTime is EventStartTime - (PrepMinutes * 60).
