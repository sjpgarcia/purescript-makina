"use strict";

exports.cataImpl = (create) => (init) => (step) => (algebra) => (structure) => {
    let currentFeed = create(structure);
    let currentIndex = currentFeed(init);
    let currentStack = [];
    while (true) {
        if (currentIndex.tag === "more") {
            currentStack.push(currentFeed)
            currentFeed = create(currentIndex.value);
            currentIndex = currentFeed(init);
        } else if (currentIndex.tag === "done") {
            if (currentStack.length === 0) {
                return algebra(currentIndex.value);
            } else {
                currentFeed = currentStack.pop();
                currentIndex = currentFeed(step(algebra(currentIndex.value)));
            }
        }
    }
}
