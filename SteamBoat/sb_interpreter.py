from os import stat
import sys
import sb_langdef

class Interpreter():
    def error(self, message):
        print(message)
        sys.exit(2)
    def non_fatal(self, message):
        print(message)
    def validate(self):
        if "England" not in self.ast:
            self.error("Fool! You forgot England!")
    def run(self, ast):
        self.ast = ast
        self.validate()
        self.ships = []
        self.running = True
        ship = {
            "name": "Steamer",
            "stack": [],
            "dest_island": "England",
            "dest_port": None,
            "current_island": "England",
            "lock": 0,
            "log": []
        }
        self.ships.append(ship)
        day = 0
        while len(self.ships) > 0:
            self.tick(day)
            day += 1
    def match_lock(self, ship):
        iname = ship["current_island"]
        island = self.ast[iname]

        pattern = island["locks"][ship["lock"]][0]
        stack = list(reversed(ship["stack"]))
        pp = 0
        sp = 0
        def match_gate(gate, stack, idx):
            if idx >= len(stack):
                return False
            val = stack[idx]
            # if gate[0] == "empty":
            #     return len(stack) == 0
            if gate[0] == "wildcard":
                return True
            elif gate[0] == "lit":
                num = gate[1]
                if sp < len(stack):
                    if val == num:
                        return True
                return False
            else:
                return False
        def match_gate_group():
            nonlocal pp, sp
            gate = pattern[pp]
            count = gate[0]
            if count == "matchall":
                count = len(stack) - sp
            elif count == "empty":
                pp += 1
                return len(stack) == 0
            elif not isinstance(count, int):
                return False
            for i in range(count):
                if not sp + i < len(stack) or not match_gate(gate[1], stack, sp + i):
                    return False
            pp += 1
            sp += count
            return True
        while pp < len(pattern):
            if pp < len(pattern) and sp < len(stack) and match_gate(pattern[pp], stack, sp):
                pp += 1
                sp += 1
            elif match_gate_group():
                pass
            else:
                return False
        return sp == len(stack)
    def valid_stack(self, ship, leng, op, lock = "anon lock", iname = "anon island", day = "[unknown]"):
        sl = len(ship["stack"])
        if sl < leng:
            mess = f"attempted to {op} with a stack of size {sl} at lock {lock}, {iname} on day {day}. The minimum stack size required is {leng}"
            ship["log"].append(mess)
            return False
        return True

    def canal(self, ship, day):
        iname = ship["current_island"]
        island = self.ast[iname]
        route = island["locks"][ship["lock"]][1]
        stack : list = ship["stack"]
        lock = ship["lock"]
        arith = {
            "add":lambda a,b:a+b,
            "sub":lambda a,b:a-b,
            "mul":lambda a,b:a*b,
            "div":lambda a,b:a//b,
            "mod":lambda a,b:a%b,
        }
        asst = {
            "aez":lambda a: a == 0,
            "alz":lambda a: a < 0,
            "agz":lambda a: a > 0,
            "anz":lambda a: a != 0,
        }
        for station in route:
            if station[0] == "lit":
                stack.append(station[1])
            elif station[0] == "in":
                island["ptr"] = (island["ptr"] - 1) % len(island["buffer"])
                stack.append(island["buffer"][island["ptr"]])
            elif station[0] == "out":
                if self.valid_stack(ship, 1, station[0], lock, iname,):
                    island["buffer"][island["ptr"]] = stack.pop()
                    island["ptr"] = (island["ptr"] + 1) % len(island["buffer"])
                else:
                    break
            elif station[0] == "dump":
                while len(stack) > 0:
                    island["buffer"][island["ptr"]] = stack.pop()
                    island["ptr"] = (island["ptr"] + 1) % len(island["buffer"])
            elif station[0] == "dupe":
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    stack.append(stack[-1])
                else:
                    break
            elif station[0] == "del":
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    stack.pop()
                else:
                    break
            elif station[0] == "swp":
                if self.valid_stack(ship, 2, station[0], lock, iname, day):
                    a = stack.pop()
                    b = stack.pop()
                    stack.append(a)
                    stack.append(b)
                else:
                    break
            elif station[0] == "rev":
                stack.reverse()
            elif station[0] == "inc":
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    stack[-1] += 1
                else:
                    break
            elif station[0] == "dec":
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    stack[-1] -= 1
                else:
                    break
            elif station[0] == "neg":
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    stack[-1] *= -1
                else:
                    break
            elif station[0] in arith:
                if self.valid_stack(ship, 2, station[0], lock, iname, day):
                    a = stack.pop()
                    b = stack.pop()
                    stack.append(arith[station[0]](b,a))
                else:
                    break
            elif station[0] in asst:
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    a = stack[-1]
                    if not asst[station[0]](a):
                        break
                else:
                    break
            elif station[0] == "goto":
                if not station[1]:
                    ship["dest_island"] = ship["current_island"]
                else:
                    ship["dest_island"] = station[1]
                ship["dest_port"] = station[2]
            elif station[0] == "status":
                self.status(ship)
            elif station[0] == "survey":
                self.survey(island)
            elif station[0] == "collate":
                buff = island["buffer"][:island["ptr"]]
                buff.reverse()
                print("".join([chr(c) for c in buff]))
            elif station[0] == "halt":
                self.ships.remove(ship)
                break
            elif station[0] == "set":
                if self.valid_stack(ship, 1, station[0], lock, iname, day):
                    a = stack.pop()
                    island["ptr"] = a
                else:
                    break
            elif station[0] == "read":
                stack.append(island["ptr"])
            elif station[0] == "inp":
                chars = input("awaiting input: ")
                for c in chars:
                    stack.append(ord(c))
            elif station[0] in sb_langdef.stations:
                self.error(f"command {station[0]} is not yet implimented")
            else:
                self.error(f"unrecognized command {station[0]}")
    def status(self, ship):
        for key, val in ship.items():
            print(key, ":", val)
        print()
    def survey(self, island):
        print(f"Name:", island["name"])
        buff = island["buffer"][:island["ptr"]]
        print("Buffer", buff)
        print("Pointer", island["ptr"])
    def tick(self, day):
        for ship in self.ships:
            # try to match current lock
            # if it fails proceed to next lock
            # otherwise go through canal
            iname = ship["current_island"]
            island = self.ast[iname]
            if ship["dest_island"]:
                ship["current_island"] = ship["dest_island"]
                ship["dest_island"] = None
            elif ship["dest_port"]:
                if island["ports"][ship["dest_port"]] != ship["lock"]:
                    ship["lock"] = island["ports"][ship["dest_port"]]
                ship["dest_port"] = None
            else:
                if self.match_lock(ship):
                    self.canal(ship, day)
                ship["lock"] += 1
                if ship["lock"] >= len(self.ast[ship["current_island"]]["locks"]):
                    ship["lock"] = 0