// Package FSM provides a generic, thread-safe finite state machine implementation
// that leverages Go's type system for compile-time safety. It can be reused across
// different projects with any state and event types.
package FSM

import (
	"fmt"
	"sync"
)

// State represents the state of the machine. It must be a comparable type
// to be used as a map key for efficient transition lookups.
type State comparable

// Event represents the events that can trigger transitions. It must be a comparable type
// to be used as a map key for efficient event matching.
type Event comparable

// ActionFunc defines the signature for functions executed during transitions.
// It takes the current state, the event, and returns the next state or an error.
// This allows actions to influence the transition outcome.
type ActionFunc[S State, E Event] func(currentState S, event E) (S, error)

// StateHookFunc defines the signature for functions executed when entering or exiting states.
// It takes the state and can return an error to prevent the state change.
type StateHookFunc[S State] func(state S) error

// Transition defines a state change rule for the FSM.
type Transition[S State, E Event] struct {
	From   S
	Event  E
	To     S
	Action ActionFunc[S, E] // Optional action to execute during the transition
}

// FSM represents the finite state machine.
// S and E are the generic types for State and Event respectively.
type FSM[S State, E Event] struct {
	// The current state of the machine.
	currentState S

	// A map to store transitions for quick lookup.
	// Key: State (From), Value: Map of Event -> Transition
	transitions map[S]map[E]Transition[S, E]

	// Hooks for state entry and exit events
	entryHooks map[S][]StateHookFunc[S]
	exitHooks  map[S][]StateHookFunc[S]

	// Mutex for thread safety if the FSM is used concurrently.
	mu sync.RWMutex
}

// NewFSM creates and initializes a new FSM instance with a specified initial state.
// S and E are type parameters inferred from the initial state and used throughout.
func NewFSM[S State, E Event](initialState S) *FSM[S, E] {
	return &FSM[S, E]{
		currentState: initialState,
		transitions:  make(map[S]map[E]Transition[S, E]),
		entryHooks:   make(map[S][]StateHookFunc[S]),
		exitHooks:    make(map[S][]StateHookFunc[S]),
	}
}

// AddTransition adds a new transition rule to the FSM.
// It defines how the machine moves from one state to another based on an event.
func (fsm *FSM[S, E]) AddTransition(t Transition[S, E]) {
	fsm.mu.Lock()
	defer fsm.mu.Unlock()

	// Ensure the map for the 'From' state exists.
	if _, ok := fsm.transitions[t.From]; !ok {
		fsm.transitions[t.From] = make(map[E]Transition[S, E])
	}
	// Add the transition under the 'From' state and 'Event' key.
	fsm.transitions[t.From][t.Event] = t
}

// CurrentState returns the machine's current state in a thread-safe manner.
func (fsm *FSM[S, E]) CurrentState() S {
	fsm.mu.RLock()
	defer fsm.mu.RUnlock()
	return fsm.currentState
}

// SendEvent triggers a transition based on the current state and the provided event.
// It returns the new state and any error that occurred during the transition.
// If the transition is invalid, it returns the current state and an error.
func (fsm *FSM[S, E]) SendEvent(event E) (S, error) {
	fsm.mu.Lock()
	defer fsm.mu.Unlock()

	// Look up possible transitions from the current state.
	transitionsFromCurrentState, ok := fsm.transitions[fsm.currentState]
	if !ok {
		// No transitions are defined from the current state.
		var zero S // Zero value of the State type
		return zero, fmt.Errorf("no transitions defined from state '%v'", fsm.currentState)
	}

	// Look up the specific transition for the given event.
	transition, ok := transitionsFromCurrentState[event]
	if !ok {
		// No transition is defined for this event from the current state.
		var zero S
		return zero, fmt.Errorf("invalid event '%v' in state '%v'", event, fsm.currentState)
	}

	// Execute the action if one is defined.
	// The action can potentially modify the destination state or cause an error.
	nextState := transition.To
	var err error
	if transition.Action != nil {
		nextState, err = transition.Action(fsm.currentState, event)
		if err != nil {
			// If the action returns an error, the transition fails.
			var zero S
			return zero, fmt.Errorf("transition action failed: %w", err)
		}
	}

	// Execute exit hooks for the current state
	if hooks, ok := fsm.exitHooks[fsm.currentState]; ok {
		for _, hook := range hooks {
			if err := hook(fsm.currentState); err != nil {
				var zero S
				return zero, fmt.Errorf("exit hook failed for state '%v': %w", fsm.currentState, err)
			}
		}
	}

	// Update the current state to the (potentially modified) destination state.
	fsm.currentState = nextState

	// Execute entry hooks for the new state
	if hooks, ok := fsm.entryHooks[fsm.currentState]; ok {
		for _, hook := range hooks {
			if err := hook(fsm.currentState); err != nil {
				var zero S
				return zero, fmt.Errorf("entry hook failed for state '%v': %w", fsm.currentState, err)
			}
		}
	}

	return fsm.currentState, nil
}

// AddEntryHook adds a hook function that will be called when entering the specified state.
func (fsm *FSM[S, E]) AddEntryHook(state S, hook StateHookFunc[S]) {
	fsm.mu.Lock()
	defer fsm.mu.Unlock()
	fsm.entryHooks[state] = append(fsm.entryHooks[state], hook)
}

// AddExitHook adds a hook function that will be called when exiting the specified state.
func (fsm *FSM[S, E]) AddExitHook(state S, hook StateHookFunc[S]) {
	fsm.mu.Lock()
	defer fsm.mu.Unlock()
	fsm.exitHooks[state] = append(fsm.exitHooks[state], hook)
}

// CanTransition checks if a transition is possible from the current state with the given event.
func (fsm *FSM[S, E]) CanTransition(event E) bool {
	fsm.mu.RLock()
	defer fsm.mu.RUnlock()

	if transitionsFromCurrentState, ok := fsm.transitions[fsm.currentState]; ok {
		_, exists := transitionsFromCurrentState[event]
		return exists
	}
	return false
}

// HasTransition checks if a specific transition exists from one state to another with the given event.
func (fsm *FSM[S, E]) HasTransition(from S, event E) bool {
	fsm.mu.RLock()
	defer fsm.mu.RUnlock()

	if transitionsFromState, ok := fsm.transitions[from]; ok {
		_, exists := transitionsFromState[event]
		return exists
	}
	return false
}

// GetValidEvents returns all valid events from the current state.
func (fsm *FSM[S, E]) GetValidEvents() []E {
	fsm.mu.RLock()
	defer fsm.mu.RUnlock()

	var events []E
	if transitionsFromCurrentState, ok := fsm.transitions[fsm.currentState]; ok {
		for event := range transitionsFromCurrentState {
			events = append(events, event)
		}
	}
	return events
}

// GetStates returns all states that have transitions defined (either as source or destination).
func (fsm *FSM[S, E]) GetStates() []S {
	fsm.mu.RLock()
	defer fsm.mu.RUnlock()

	stateSet := make(map[S]bool)

	// Add current state
	stateSet[fsm.currentState] = true

	// Add all states from transitions
	for from, transitions := range fsm.transitions {
		stateSet[from] = true
		for _, transition := range transitions {
			stateSet[transition.To] = true
		}
	}

	var states []S
	for state := range stateSet {
		states = append(states, state)
	}
	return states
}

// AddTransitions adds multiple transitions at once for bulk operations.
func (fsm *FSM[S, E]) AddTransitions(transitions []Transition[S, E]) {
	fsm.mu.Lock()
	defer fsm.mu.Unlock()

	for _, t := range transitions {
		// Ensure the map for the 'From' state exists.
		if _, ok := fsm.transitions[t.From]; !ok {
			fsm.transitions[t.From] = make(map[E]Transition[S, E])
		}
		// Add the transition under the 'From' state and 'Event' key.
		fsm.transitions[t.From][t.Event] = t
	}
}
