

$(document).ready(() => {
    const t = new Test(() => alert("test"));
    //t.go();
});

class Test {
    public constructor (public go: () => void){

    }
}