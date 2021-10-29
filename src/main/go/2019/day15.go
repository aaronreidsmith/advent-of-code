// I am not smart enough to figure this out myself. Adapted from: https://git.io/JP4aH
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const ramSize = 8192

var puzzleInput = os.Args[1]

type (
	Machine struct {
		pc       int
		mem      []int
		inp      chan int
		out      chan int
		outGauge int
		relBase  int
	}
)

func NewMachine(mem []int) *Machine {
	return &Machine{
		pc:       0,
		mem:      mem,
		inp:      make(chan int, 2),
		out:      make(chan int, 2),
		outGauge: 0,
		relBase:  0,
	}
}

const (
	modePos = iota
	modeImm
	modeRel
)

func paramMode(mode int) int {
	switch mode {
	case 0:
		return modePos
	case 1:
		return modeImm
	case 2:
		return modeRel
	default:
		log.Fatal("Illegal param mode", mode)
		return 0
	}
}

func decodeOp(code int) (int, int, int, int) {
	op := code % 100
	mode1 := paramMode(code / 100 % 10)
	mode2 := paramMode(code / 1000 % 10)
	mode3 := paramMode(code / 10000 % 10)
	return op, mode1, mode2, mode3
}

func (m *Machine) getMem(pos int) int {
	return m.mem[pos]
}

func (m *Machine) evalArg(mode int, arg int) int {
	switch mode {
	case modePos:
		return m.getMem(arg)
	case modeImm:
		return arg
	case modeRel:
		return m.getMem(arg + m.relBase)
	default:
		log.Fatal("Illegal arg mode")
		return 0
	}
}

func (m *Machine) getArg(mode, offset int) int {
	arg := m.getMem(m.pc + offset)
	return m.evalArg(mode, arg)
}

func (m *Machine) setMem(pos, val int) {
	m.mem[pos] = val
}

func (m *Machine) setArg(mode, offset, val int) {
	arg := m.getMem(m.pc + offset)
	switch mode {
	case modePos:
		m.setMem(arg, val)
	case modeImm:
		log.Fatal("Illegal mem write imm mode")
	case modeRel:
		m.setMem(arg+m.relBase, val)
	default:
		log.Fatal("Illegal mem write mode")
	}
}

func (m *Machine) stepPC(offset int) {
	m.pc += offset
}

func (m *Machine) Write(inp int) {
	m.inp <- inp
}

func (m *Machine) recvInput() int {
	return <-m.inp
}

func (m *Machine) sendOutput(out int) {
	m.outGauge = out
	m.out <- out
}

func (m *Machine) Read() (int, bool) {
	v, ok := <-m.out
	if !ok {
		return 0, false
	}
	return v, true
}

func (m *Machine) Exec() bool {
	op, a1, a2, a3 := decodeOp(m.getMem(m.pc))
	switch op {
	case 1:
		m.setArg(a3, 3, m.getArg(a1, 1)+m.getArg(a2, 2))
		m.stepPC(4)
	case 2:
		m.setArg(a3, 3, m.getArg(a1, 1)*m.getArg(a2, 2))
		m.stepPC(4)
	case 3:
		m.setArg(a1, 1, m.recvInput())
		m.stepPC(2)
	case 4:
		m.sendOutput(m.getArg(a1, 1))
		m.stepPC(2)
	case 5:
		if m.getArg(a1, 1) != 0 {
			m.pc = m.getArg(a2, 2)
		} else {
			m.stepPC(3)
		}
	case 6:
		if m.getArg(a1, 1) == 0 {
			m.pc = m.getArg(a2, 2)
		} else {
			m.stepPC(3)
		}
	case 7:
		if m.getArg(a1, 1) < m.getArg(a2, 2) {
			m.setArg(a3, 3, 1)
		} else {
			m.setArg(a3, 3, 0)
		}
		m.stepPC(4)
	case 8:
		if m.getArg(a1, 1) == m.getArg(a2, 2) {
			m.setArg(a3, 3, 1)
		} else {
			m.setArg(a3, 3, 0)
		}
		m.stepPC(4)
	case 9:
		m.relBase += m.getArg(a1, 1)
		m.stepPC(2)
	case 99:
		m.stepPC(1)
		return false
	default:
		log.Fatal("Illegal op code", m.pc, m.getMem(m.pc))
	}
	return true
}

func (m *Machine) Execute() {
	for m.Exec() {
	}
	close(m.out)
}

const (
	dirNorth = 1
	dirSouth = 2
	dirWest  = 3
	dirEast  = 4
)

const (
	statusWall = 0
	statusMove = 1
	statusGoal = 2
)

type (
	Point struct {
		x, y int
	}

	Bot struct {
		x, y      int
		commands  []int
		m         *Machine
		closedSet map[Point]struct{}
	}
)

func NewBot(m *Machine) *Bot {
	return &Bot{
		x:         0,
		y:         0,
		commands:  []int{},
		m:         m,
		closedSet: map[Point]struct{}{},
	}
}

func (r *Bot) north() {
	r.y -= 1
	r.commands = append(r.commands, dirNorth)
	r.closedSet[Point{r.x, r.y}] = struct{}{}
}
func (r *Bot) south() {
	r.y += 1
	r.commands = append(r.commands, dirSouth)
	r.closedSet[Point{r.x, r.y}] = struct{}{}
}
func (r *Bot) west() {
	r.x -= 1
	r.commands = append(r.commands, dirWest)
	r.closedSet[Point{r.x, r.y}] = struct{}{}
}
func (r *Bot) east() {
	r.x += 1
	r.commands = append(r.commands, dirEast)
	r.closedSet[Point{r.x, r.y}] = struct{}{}
}
func (r *Bot) pop() {
	last := r.commands[len(r.commands)-1]
	r.commands = r.commands[:len(r.commands)-1]
	cmd := 0
	switch last {
	case dirNorth:
		cmd = dirSouth
		r.y += 1
	case dirSouth:
		cmd = dirNorth
		r.y -= 1
	case dirWest:
		cmd = dirEast
		r.x += 1
	case dirEast:
		cmd = dirWest
		r.x -= 1
	default:
		log.Fatalln("Illegal command")
	}
	r.m.Write(cmd)
	k, ok := r.m.Read()
	if !ok || k == 0 {
		log.Fatalln("Bot crashed on reverse")
	}
}

func (r *Bot) tryNorth() int {
	r.m.Write(dirNorth)
	k, ok := r.m.Read()
	if !ok {
		log.Fatalln("Bot crashed")
	}
	return k
}
func (r *Bot) trySouth() int {
	r.m.Write(dirSouth)
	k, ok := r.m.Read()
	if !ok {
		log.Fatalln("Bot crashed")
	}
	return k
}
func (r *Bot) tryWest() int {
	r.m.Write(dirWest)
	k, ok := r.m.Read()
	if !ok {
		log.Fatalln("Bot crashed")
	}
	return k
}
func (r *Bot) tryEast() int {
	r.m.Write(dirEast)
	k, ok := r.m.Read()
	if !ok {
		log.Fatalln("Bot crashed")
	}
	return k
}

func (r *Bot) IDS(size int, target int) bool {
	if size == 0 {
		return false
	}

	last := -1
	if len(r.commands) > 0 {
		last = r.commands[len(r.commands)-1]
	}

	if last != dirSouth {
		if k := r.tryNorth(); k == target {
			r.north()
			return true
		} else if k == 1 {
			r.north()
			if r.IDS(size-1, target) {
				return true
			}
			r.pop()
		} else if k == 0 {
		} else {
			log.Fatalln("Bot crashed: illegal status")
		}
	}
	if last != dirNorth {
		if k := r.trySouth(); k == target {
			r.south()
			return true
		} else if k == 1 {
			r.south()
			if r.IDS(size-1, target) {
				return true
			}
			r.pop()
		} else if k == 0 {
		} else {
			log.Fatalln("Bot crashed: illegal status")
		}
	}
	if last != dirEast {
		if k := r.tryWest(); k == target {
			r.west()
			return true
		} else if k == 1 {
			r.west()
			if r.IDS(size-1, target) {
				return true
			}
			r.pop()
		} else if k == 0 {
		} else {
			log.Fatalln("Bot crashed: illegal status")
		}
	}
	if last != dirWest {
		if k := r.tryEast(); k == target {
			r.east()
			return true
		} else if k == 1 {
			r.east()
			if r.IDS(size-1, target) {
				return true
			}
			r.pop()
		} else if k == 0 {
		} else {
			log.Fatalln("Bot crashed: illegal status")
		}
	}

	return false
}

func (r *Bot) Explore() int {
	for i := 0; true; i++ {
		if r.IDS(i, 2) {
			return i
		}
	}
	return -1
}

func (r *Bot) Explore2() int {
	size := len(r.closedSet)
	for i := 1; true; i++ {
		r.IDS(i, -1)
		k := len(r.closedSet)
		if k <= size {
			return i - 1
		}
		size = k
	}
	return -1
}

func (r *Bot) Erase() {
	r.commands = []int{}
	r.closedSet = map[Point]struct{}{}
}

func main() {
	tokens := []int{}

	{
		file, err := os.Open(puzzleInput)
		if err != nil {
			log.Fatal(err)
		}
		defer func() {
			if err := file.Close(); err != nil {
				log.Fatal(err)
			}
		}()

		scanner := bufio.NewScanner(file)
		for scanner.Scan() {
			nums := strings.Split(scanner.Text(), ",")
			for _, i := range nums {
				num, err := strconv.Atoi(i)
				if err != nil {
					log.Fatal(err)
				}
				tokens = append(tokens, num)
			}
		}

		if err := scanner.Err(); err != nil {
			log.Fatal(err)
		}
	}

	{
		mem := make([]int, ramSize)
		copy(mem, tokens)
		m := NewMachine(mem)
		go m.Execute()
		r := NewBot(m)
		fmt.Println("Part 1:", r.Explore())
		r.Erase()
		fmt.Println("Part 2:", r.Explore2())
	}
}
