"use strict";

exports.createImpl = (structure) => (destructure) => (restructure) => {
    let { tag, fields, points } = destructure(structure);

    let inputStack = points;
    let outputStack = [];

    return (message) => {
        if (message.tag === "step") {
            outputStack.push(message.value);
        } else {

        }
        if (inputStack.length === 0) {
            return { tag: "done", value: restructure({ tag: tag, fields: fields, points: outputStack.reverse() }) }
        } else {
            return { tag: "more", value: inputStack.pop() }
        }
    };
}
