class Controller {
    public async get(@Args() { id, credits }: GetArgs): Promise<Artist> {
        const url = `{id}`
        const b = callme();
    } 
}
    
async function f() {
  return 1;
}
let value = await promise;