const isGroupStart = code => code[0] == "["

const eatEnclosed = (code, endingChar) => {
    let enclosedCode = "";
    let restCode = code.slice(1);
    while (restCode && restCode[0] !== endingChar) {
        enclosedCode += restCode[0];
        restCode = restCode.slice(1);
    }
    restCode = restCode.slice(1);
    return [restCode, enclosedCode];
}

const eatGroup = code => eatEnclosed(code, "]");

const groupToCommands = code => {
    return code.split(/(\(.*?\)|\s+)/).filter(v => v.trim()).map(v => isNaN(v) ? v : parseInt(v));
}

const optionallyEatRepetitions = code => {
    let repetitionsString = "";
    let restCode = code;
    while (restCode && restCode[0] && !isNaN(restCode[0])) {
        repetitionsString += restCode[0];
        restCode = restCode.slice(1);
    }
    return [restCode, repetitionsString ? parseInt(repetitionsString) : null];
}

const codeToGroups = code => {
    let restCode = code.trim();
    let groups = [];
    while (restCode) {
        let [nCode, commands] = eatGroup(restCode);
        let [rCode, repetitions] = optionallyEatRepetitions(nCode);
        groups.push({
            commands,
            repetitions: repetitions
        });
        restCode = rCode.trim();
    }
    return groups;
}

const isCodeBlock = command => command[0] == "(";
const eatCodeBlock = code => eatEnclosed(code, ")");

const parseCode = code => {
    const groups = codeToGroups(code);
    let parsed = {};
    groups.forEach((g, i) => {
        parsed[i] = {
            commands: groupToCommands(g.commands).map(c => isCodeBlock(c) ? groupToCommands(eatCodeBlock(c)[1]) : c),
            repetitions: g.repetitions ? g.repetitions : 1
        };
    });
    return parsed;
}

exports.parseCode = parseCode;
