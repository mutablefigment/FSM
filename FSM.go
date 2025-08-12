package FSM

import "fmt"

type State string

type Event string

const (
	StateOff State = "off"
	StateOn  State = "on"

	EventSwitch Event = "switch"
)

type Transition struct {
	From   State
	Event  Event
	To     State
	Action func()
}

type StateMachine struct {
	Current     State
	Transitions map[State]map[Event]Transition
}

func NewStateMachine(initialState State) *StateMachine {
	return &StateMachine{
		Current:     initialState,
		Transitions: make(map[State]map[Event]Transition),
	}
}

func (sm *StateMachine) AddTransition(t Transition) {
	if _, ok := sm.Transitions[t.From]; !ok {
		sm.Transitions[t.From] = make(map[Event]Transition)
	}
	sm.Transitions[t.From][t.Event] = t
}

func (sm *StateMachine) SendEvent(event Event) error {
	transitionsFromCurrentState, ok := sm.Transitions[sm.Current]
	if !ok {
		return fmt.Errorf("no transitions defined from state '%s'", sm.Current)
	}

	transition, ok := transitionsFromCurrentState[event]
	if !ok {
		return fmt.Errorf("invalid event '%s' in state '%s'", event, sm.Current)
	}

	if transition.Action != nil {
		transition.Action()
	}

	sm.Current = transition.To

	return nil
}
