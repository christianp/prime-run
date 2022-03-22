export class PrimeGenerator {
    constructor() {
        this.primes = [2];
        this.n = 3;
        this._prime_factors = {};
    }

    * fn(upto) {
        for(let p of this.primes) {
            if(p>upto) {
                return;
            }
            yield p;
        }
        while(this.n<=upto) {
            if(!this.primes.some(p=>this.n%p==0)) {
                this.primes.push(this.n);
                yield this.n;
            }
            this.n += 2;
        }
    }

    is_prime(n) {
        for(let p of this.fn(n)) {
            if(p<n && n%p==0) {
                return false;
            }
            if(p*p>n) {
                break;
            }
        }
        return true;
    }

    prime_factors(n) {
        if(this._prime_factors[n]===undefined) {
            const l = [];
            for(let p of this.fn(n)) {
                if(n%p==0) {
                    l.push(p);
                }
            }
            this._prime_factors[n] = l;
        }
        return this._prime_factors[n];
    }
}

const g = new PrimeGenerator();

export function par(current,target) {
    const settled = {};
    const dist = {};
    //path = {};
    //path[current] = [current];
    dist[current] = 0;
    let queue = [current];

    const t1 = new Date();

    function change(x,y,d) {
      if(y<2) {
        return;
      }
      if(settled[y]) {
        return;
      }
      dist[y] = (y in dist) ? Math.min(dist[y],d) : d;
    //  path[y] = path[x].concat([y]);
      queue = queue.filter(z=>z!=y);
      for(let i=0;i<queue.length;i++) {
        if(dist[queue[i]] >= d) {
          queue.splice(i,0,y);
          return;
        }
      }
      queue.push(y);
    }

    return new Promise((resolve,reject) => {
        let steps = 0;
        function frame() {
            for(let i=0;i<100;i++) {
              const x = queue.shift();
              //console.log(queue.slice());
              steps += 1;
              if(settled[x]) {
                continue;
              }
              const d = dist[x];
              settled[x] = true;
              if(x==target) {
                break;
              }
              const factors = g.prime_factors(x);
              for(let f of factors){
                change(x,x+f,d+1);
                if(f==x) {
                  continue;
                }
                change(x,x-f,d+1);
              }
            }
            if(!settled[target]) {
                requestAnimationFrame(frame);
            } else {
                const t2 = new Date();
                console.log(`took ${(t2-t1)/1000}`);
                resolve(dist[target]);
            }
        }

        requestAnimationFrame(frame);
    });
}
