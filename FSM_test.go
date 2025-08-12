package FSM

import (
	"fmt"
	"testing"
)

// Test states and events for testing
type TestState string
type TestEvent string

const (
	StateIdle    TestState = "idle"
	StateRunning TestState = "running"
	StateStopped TestState = "stopped"
	StateError   TestState = "error"
)

const (
	EventStart TestEvent = "start"
	EventStop  TestEvent = "stop"
	EventError TestEvent = "error"
	EventReset TestEvent = "reset"
)

func TestNewFSM(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)
	if fsm.CurrentState() != StateIdle {
		t.Errorf("Expected initial state to be %v, got %v", StateIdle, fsm.CurrentState())
	}
}

func TestAddTransition(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	// Add a simple transition
	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})

	// Test the transition
	newState, err := fsm.SendEvent(EventStart)
	if err != nil {
		t.Errorf("Unexpected error: %v", err)
	}
	if newState != StateRunning {
		t.Errorf("Expected state %v, got %v", StateRunning, newState)
	}
}

func TestAddTransitions(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	transitions := []Transition[TestState, TestEvent]{
		{From: StateIdle, Event: EventStart, To: StateRunning},
		{From: StateRunning, Event: EventStop, To: StateStopped},
		{From: StateStopped, Event: EventReset, To: StateIdle},
	}

	fsm.AddTransitions(transitions)

	// Test the transitions
	newState, err := fsm.SendEvent(EventStart)
	if err != nil || newState != StateRunning {
		t.Errorf("First transition failed: state=%v, err=%v", newState, err)
	}

	newState, err = fsm.SendEvent(EventStop)
	if err != nil || newState != StateStopped {
		t.Errorf("Second transition failed: state=%v, err=%v", newState, err)
	}

	newState, err = fsm.SendEvent(EventReset)
	if err != nil || newState != StateIdle {
		t.Errorf("Third transition failed: state=%v, err=%v", newState, err)
	}
}

func TestTransitionWithAction(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	actionCalled := false
	action := func(currentState TestState, event TestEvent) (TestState, error) {
		actionCalled = true
		if currentState != StateIdle || event != EventStart {
			return StateError, fmt.Errorf("unexpected action parameters")
		}
		return StateRunning, nil
	}

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:   StateIdle,
		Event:  EventStart,
		To:     StateRunning,
		Action: action,
	})

	newState, err := fsm.SendEvent(EventStart)
	if err != nil {
		t.Errorf("Unexpected error: %v", err)
	}
	if newState != StateRunning {
		t.Errorf("Expected state %v, got %v", StateRunning, newState)
	}
	if !actionCalled {
		t.Error("Action was not called")
	}
}

func TestTransitionActionError(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	action := func(currentState TestState, event TestEvent) (TestState, error) {
		return StateError, fmt.Errorf("action error")
	}

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:   StateIdle,
		Event:  EventStart,
		To:     StateRunning,
		Action: action,
	})

	_, err := fsm.SendEvent(EventStart)
	if err == nil {
		t.Error("Expected error from action, got nil")
	}

	// State should remain unchanged
	if fsm.CurrentState() != StateIdle {
		t.Errorf("State should remain %v, got %v", StateIdle, fsm.CurrentState())
	}
}

func TestInvalidTransition(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	// Try to send an event without any transitions
	_, err := fsm.SendEvent(EventStart)
	if err == nil {
		t.Error("Expected error for invalid transition, got nil")
	}

	// Add a transition and try an invalid event
	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})

	_, err = fsm.SendEvent(EventStop)
	if err == nil {
		t.Error("Expected error for invalid event, got nil")
	}
}

func TestHooks(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	var entryCallLog []TestState
	var exitCallLog []TestState

	entryHook := func(state TestState) error {
		entryCallLog = append(entryCallLog, state)
		return nil
	}

	exitHook := func(state TestState) error {
		exitCallLog = append(exitCallLog, state)
		return nil
	}

	fsm.AddEntryHook(StateRunning, entryHook)
	fsm.AddExitHook(StateIdle, exitHook)

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})

	_, err := fsm.SendEvent(EventStart)
	if err != nil {
		t.Errorf("Unexpected error: %v", err)
	}

	// Check that hooks were called
	if len(exitCallLog) != 1 || exitCallLog[0] != StateIdle {
		t.Errorf("Exit hook not called correctly: %v", exitCallLog)
	}
	if len(entryCallLog) != 1 || entryCallLog[0] != StateRunning {
		t.Errorf("Entry hook not called correctly: %v", entryCallLog)
	}
}

func TestHookError(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	errorHook := func(state TestState) error {
		return fmt.Errorf("hook error")
	}

	fsm.AddEntryHook(StateRunning, errorHook)

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})

	_, err := fsm.SendEvent(EventStart)
	if err == nil {
		t.Error("Expected error from hook, got nil")
	}
}

func TestCanTransition(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	// Should return false when no transitions are defined
	if fsm.CanTransition(EventStart) {
		t.Error("Expected false for non-existent transition")
	}

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})

	// Should return true for valid transition
	if !fsm.CanTransition(EventStart) {
		t.Error("Expected true for valid transition")
	}

	// Should return false for invalid event
	if fsm.CanTransition(EventStop) {
		t.Error("Expected false for invalid event")
	}
}

func TestHasTransition(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	// Should return false when no transitions are defined
	if fsm.HasTransition(StateIdle, EventStart) {
		t.Error("Expected false for non-existent transition")
	}

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})

	// Should return true for existing transition
	if !fsm.HasTransition(StateIdle, EventStart) {
		t.Error("Expected true for existing transition")
	}

	// Should return false for non-existing transition
	if fsm.HasTransition(StateRunning, EventStart) {
		t.Error("Expected false for non-existing transition")
	}
}

func TestGetValidEvents(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	// Should return empty slice when no transitions are defined
	events := fsm.GetValidEvents()
	if len(events) != 0 {
		t.Errorf("Expected empty events slice, got %v", events)
	}

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})
	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventError,
		To:    StateError,
	})

	events = fsm.GetValidEvents()
	if len(events) != 2 {
		t.Errorf("Expected 2 events, got %d", len(events))
	}

	// Check that both events are present
	eventMap := make(map[TestEvent]bool)
	for _, event := range events {
		eventMap[event] = true
	}
	if !eventMap[EventStart] || !eventMap[EventError] {
		t.Errorf("Expected EventStart and EventError, got %v", events)
	}
}

func TestGetStates(t *testing.T) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	// Should include at least the current state
	states := fsm.GetStates()
	if len(states) != 1 || states[0] != StateIdle {
		t.Errorf("Expected [%v], got %v", StateIdle, states)
	}

	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})
	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateRunning,
		Event: EventStop,
		To:    StateStopped,
	})

	states = fsm.GetStates()
	if len(states) != 3 {
		t.Errorf("Expected 3 states, got %d", len(states))
	}

	// Check that all states are present
	stateMap := make(map[TestState]bool)
	for _, state := range states {
		stateMap[state] = true
	}
	if !stateMap[StateIdle] || !stateMap[StateRunning] || !stateMap[StateStopped] {
		t.Errorf("Expected StateIdle, StateRunning, StateStopped, got %v", states)
	}
}

// Benchmark tests
func BenchmarkSendEvent(b *testing.B) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)
	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateIdle,
		Event: EventStart,
		To:    StateRunning,
	})
	fsm.AddTransition(Transition[TestState, TestEvent]{
		From:  StateRunning,
		Event: EventStop,
		To:    StateIdle,
	})

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if i%2 == 0 {
			_, err := fsm.SendEvent(EventStart)
			if err != nil {
				return
			}
		} else {
			_, err := fsm.SendEvent(EventStop)
			if err != nil {
				return
			}
		}
	}
}

func BenchmarkCurrentState(b *testing.B) {
	fsm := NewFSM[TestState, TestEvent](StateIdle)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = fsm.CurrentState()
	}
}
