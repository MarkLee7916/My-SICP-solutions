// Since chapter 3 is based primarily on mutable state and assignment, Haskell is no longer a viable language as my knowledge
// of modeling mutable state using monads inside quite there yet.

// I'll write my answers for chapter 3 in JavaScript as it has support for imperative features like assignment and mutable state,
// but also has support for functional features like llamdas and higher order functions.


//Ex 3.1/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function createAccumulator(initialValue) {
	return arg => {
		initialValue += arg;
		return initialValue;
	} 
}


//Ex 3.2/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function makeMonitored(f) {
	let counter = 0;

	return arg => {
		switch (arg) {
			case "howManyCalls?":
				return counter;
			case "resetCount":
				counter = 0;
				break;
			default:
				counter++;
				return f(arg);
		}
	}
}


//Ex 3.3, 3.4/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function makeAccount(balance, secretPassword) {
	const wrongPasswordAccessThreshold = 7;
	let wrongPasswordAccesses = 0;

	const withdraw = amount => {
		if (amount > balance) {
			return "Amount too high ";
		} else {
			balance -= amount;
			return balance;
		}
	}

	const deposit = amount => {
		balance += amount;
		return balance;
	}

	const callThePolis = () => "Weeee woooo weeee wooo";

	return (secretPasswordArg, arg) => {
		if (secretPasswordArg !== secretPassword) {
			wrongPasswordAccesses++;

			if (wrongPasswordAccesses === wrongPasswordAccessThreshold) {
				callThePolis();
				return "Your criminal days are over lawbreaker";
			}

			return "Incorrect password";
		} 

		switch (arg) {
			case "withdraw":
				return withdraw;
			case "deposit":
				return deposit;
			default:
				return "Operation not supported";
		}
	}
}


//Ex 3.5/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function genPi() {
	const predicate = (x, y) => Math.pow(x, 2) + Math.pow(y, 2) <= 1;

	return 4 * estimateIntegral(predicate, -1, 1, -1, 1, 10000000);
}

function estimateIntegral(predicate, rectXLower, rectXUpper, rectYLower, rectYUpper, trials) {
	return monteCarlo(trials, () => predicate(randomNumBetween(rectXLower, rectXUpper), randomNumBetween(rectYLower, rectYUpper)));
}

function randomNumBetween(lower, upper) {
	return Math.random() * (upper - lower) + lower;
}

// Here I use a while loop as a recursive function will hit the max call stack size
function monteCarlo(trials, experiment) {
	function iter(trialsRemaining, trialsPassed) {
		while (trialsRemaining > 0) {
			if (experiment()) {
				trialsPassed++;
			}

			trialsRemaining--;
		}

		return trialsPassed / trials;
	}

	return iter(trials, 0);
}


//Ex 3.6/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function rand() {
	const originalSeedVal = 10000;
	let seed = originalSeedVal;

	return symbol => {
		switch (symbol) {
			case "generate":
				return random();		
			case "reset":
				seed = originalSeedVal;
				break;			
			default:
				return "Operation not supported";
		}
	}

	function random() {
		let x = Math.sin(seed++) * 10000;
		return x - Math.floor(x);
	}
}


//Ex 3.7/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function makeAccount(balance, initialPassword) {
	const passwords = new Set([initialPassword]);

	const withdraw = amount => {
		if (amount > balance) {
			return "Amount too high ";
		} else {
			balance -= amount;
			return balance;
		}
	}

	const deposit = amount => {
		balance += amount;
		return balance;
	}

	const addPassword = secretPassword => {
		passwords.add(secretPassword);
	}

	return (secretPassword, arg) => {
		if (!passwords.has(secretPassword)) {
			return "Incorrect password";
		} 

		switch (arg) {
			case "withdraw":
				return withdraw;
			case "deposit":
				return deposit;
			case "addPassword":
				return addPassword;
			default:
				return "Operation not supported";
		}
	}
}

function makeJoint(account, currentPassword, newPasswordToAdd) {
	return account(currentPassword, "addPassword")(newPasswordToAdd);
}


//Ex 3.8/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function evalOrderMixup() {
	let internalState = 0;

	return arg => {
		if (arg > 0 && internalState === 0) {				
			return 1;
		}

		internalState = 1;
		return 0;
	}
}


//Ex 3.9/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Recursive process:
    // When we evaluate factorial 6, interpreter will check current environment (global in this case)
    // It will find a binding that maps "factorial" onto the body of a procedure object, with a pointer to the factorial function body
    // It will retrieve the body and apply 6 to it, creating a new environment (let's call it E1)
	// E1 will point to the environment the procedure object is pointing to, which in this case is the global environment

	// Inside E1 it will execute the code in the body line by line, substituting n for 6
	// When factorial is evaluated again, it will check E1 for a binding that maps factorial onto some value 
    // Upon not finding it, it will access E1's pointer to the global environment and find the binding
	// It will then create another environment E2, which points to the global environment as the procedure object that factorial points to points to the global environment
	// It will repeat this process until it hits a procedure where no other procedure is evaluated (the base case)

// Iterative process:
	// The process is similar, the enclosing environment for any instance of factorial-iter is also the global environment
    // The is only one environment for factorial, and 6 environments for factorial-iter
	// Despite being an invariant, they'll still be 6 copies of max-counter


//Ex 3.10////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The main difference between the two is that we're essentially wrapping a self executing llamda around the body of make-withdraw in the second example
// For the environment model this means the creation of an extra frame where initial-amount: 100 
// The set! call will still affect the balance variable


//Ex 3.11////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// I've included a picture of my diagram in the GitHub repo
// The local states are all saved in local frames that inherit from the original make-account frame
// if make-account is called twice, you'll have two seperate inheritance trees that both have their own local state
// Both frame trees will use the same global environment


//Ex 3.12////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Response 1: returns (b) as no mutation has happened. Car and cdr have created deep copies meaning x hasn't been affected at all
// Response 2: returns (a b c d) as x has been mutated by append!. In fact x and w are both pointing to the same object
// Diagrams will be included in the GitHub repo.


//Ex 3.13////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Diagrams will be included in the GitHub repo.
// It will recurse infinitely, travelling around the cycle of pointers


//Ex 3.14////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Given a list, this procedure will return a reversed version of the list
// v will print () i.e null
// w will print (d c b a)


//Ex 3.15////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Answer is in GitHub repo


//Ex 3.16////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Answer is in GitHub repo


//Ex 3.17////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function countPairs(pair) {
	const cache = new Set();

	return countPairsHelper(cache, pair);
}

function countPairsHelper(cache, pair) {
	if (!isPair(pair) || cache.has(pair)) {
		return 0;
	}

	cache.add(pair);

	return countPairsHelper(cache, car(pair)) + countPairsHelper(cache, cdr(pair)) + 1;	
}

function isPair(elem) {
	return Array.isArray(elem) && elem.length === 2;
}

function cons(elem1, elem2) {
	return [elem1, elem2];
}

function car([elem1, elem2]) {
	return elem1;
}

function cdr([elem1, elem2]) {
	return elem2;
}

function setCar(pair, arg) {
	pair[0] = arg;
}

function setCdr(pair, arg) {
	pair[1] = arg;
}

//Ex 3.18////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function isCycle(pair) {
	const cache = new Set();

	return isCycleHelper(cache, pair);
}

function isCycleHelper(cache, pair) {
	if (cache.has(pair)) {
		return true;
	}

	if (!isPair(pair)) {
		return false;
	}

	cache.add(pair);

	return isCycleHelper(cache, cdr(pair));
}

function makeCycle(pair) {
	lastPair(pair)[1] = pair;

	return pair;
}

function lastPair(pair) {
	if (!isPair(cdr(pair))) {
		return pair;
	}

	return lastPair(cdr(pair));
}


//Ex 3.19////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function isCycleConstantSpace(pair) {
	if (!isPair(pair)) {
		return false;
	}

	const slowPointer = pair;
	const fastPointer = cdr(pair);

	return isCycleConstantSpaceHelper(slowPointer, fastPointer);
}

function isCycleConstantSpaceHelper(slowPointer, fastPointer) {
	if (!isPair(fastPointer) || !isPair(cdr(fastPointer))) {
		return false;
	}

	if (slowPointer === fastPointer) {
		return true;
	}

	return isCycleConstantSpaceHelper(cdr(slowPointer), cdr(cdr(fastPointer)));
}


//Ex 3.21////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// A Queue is defined as 2 pointers
// The first pointer points to the start of the list while the second pointer points to the last item
// So whenever we try to print the list we get (actual list, end pointer)

function printQueue(queue) {
	console.log(car(queue));
}

//Ex 3.22////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function makeQueue() {
	// front is where we delete from
	let front = [];

	// rear is where we insert new elements
	let rear;

	function insert(elem) {
		if (isEmpty()) {
			front = [elem, []];
			rear = front;
		} else {
			setCdr(rear, [elem, []]);
			rear = cdr(rear);
		}		
	}

	function poll() {
		let returnElem;

		if (isEmpty()) {
			throw "Can't poll from empty queue";
		}

		returnElem = car(front);
		front = cdr(front);

		return returnElem;
	}

	function peek() {
		if (isEmpty()) {
			throw "Can't peek at empty queue";
		} 

		returnElem = car(front);
		return returnElem;
	}

	function isEmpty() {
		return front.length === 0;
	}

	return (operation, ...args) => {
		switch (operation) {
			case "insert":
				insert(args[0]);
				break;
			case "poll":
				return poll();
			case "peek":
				return peek();
			case "isEmpty":
				return isEmpty();
			default:
				throw "Operation not supported";
		}
	}
}


//Ex 3.23////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Lists in scheme are similar to singly linked lists
// pollRear() in a singly linked list is impossible in O(1) time
// Will need to find a way to let elements in the list point to the previous element

// Possible ideas:
    // Use a HashMap to map an elem onto its previous element
    // Elements are node objects instead of pairs -> can have a next and a prev pointer
        // To implement this in JS we could use triads i.e [prev, elem, next] instead of pairs

function makeDeque() {
	let front = [];
	let rear;

	function insertRear(elem) {
		if (isEmpty()) {
			front = [[], elem, []];
			rear = front;
		} else {
			setNext(rear, [rear, elem, []]);
			rear = next(rear);
		}		
	}

	function insertFront(elem) {
		if (isEmpty()) {
			front = [[], elem, []];
			rear = front;
		} else {
			const newFront = [[], elem, front];
			setPrev(front, newFront);
			front = newFront;
		}
	}

	function pollRear() {
		let returnElem;

		if (isEmpty()) {
			throw "Can't poll from empty queue";
		}

		returnElem = val(rear);
		rear = prev(rear);

		return returnElem;
	}

	function pollFront() {
		let returnElem;

		if (isEmpty()) {
			throw "Can't poll from empty queue";
		}

		returnElem = val(front);
		front = next(front);

		return returnElem;
	}
	
	function isEmpty() {
		return front.length === 0;
	}

	// We can't print normally because it will result in an infinite cycle
	function print() {
		printHelper(front);
	}

	function printHelper(node) {
		if (node.length > 0) {
			console.log(val(node));
			printHelper(next(node));
		}
	}

	function prev(node) {
		return node[0];
	}

	function next(node) {
		return node[2];
	}

	function setPrev(node, val) {
		node[0] = val;
	}

	function setNext(node, val) {
		node[2] = val;
	}

	function val(node) {
		return node[1];
	}

	return (operation, ...args) => {
		switch (operation) {
			case "insertRear":
				insertRear(args[0]);
				break;
			case "insertFront":
				insertFront(args[0]);
				break;
			case "print":
				print();
				break;
			case "pollFront":
				return pollFront();
			case "pollRear":
				return pollRear();
			case "isEmpty":
				return isEmpty();			
			default:
				throw "Operation not supported";
		}
	}
}


//Ex 3.24////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function makeTable(sameKey) {
	const table = ["dummy", null];

	function assoc(key, node) {
		if (node === null) {
			return false;
		}

		if (sameKey(car(car(node)), key)) {
			return car(node);
		} 

		return assoc(key, cdr(node));
	}
	
	function lookup(key) {
		const record = assoc(key, cdr(table));

		if (record) {
			return cdr(record);
		}

		return false;
	}

	function insert(key, val) {
		const record = assoc(key, cdr(table));

		if (record) {
			setCdr(record, val);
		} else { 
			const newNode = [key, val];
			const newEntry = [newNode, null];

			setCdr(newEntry, cdr(table));
			setCdr(table, newEntry);		
		}
	}

	function print() {
		console.log(table);

		printHelper(table);
	}

	function printHelper(node) {
		if (node !== null) {
			console.log(car(node));
			printHelper(cdr(node));
		}
	}

	return (operation, ...args) => {
		switch (operation) {
			case "lookup":
				return lookup(args[0]);
			case "insert":
				insert(args[0], args[1]);
				break;
			case "print":
				print();
				break;
			default:
				throw "Operation not supported";
		}
	}
}


//Ex 3.25////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function makeNTable(sameKey) {
	const table = ["dummy", null];
	
	function assoc(keySet, table) {
		if (keySet.length === 0) {
			return car(table);
		} else {			
			const nextTable = table("lookup", car(keySet));

			if (nextTable) {
				return assoc(cdr(keySet), nextTable);
			}

			return false;
		}	
	}	

	function lookup(keySet) {
		const record = assoc(keySet, cdr(table));

		if (record) {
			return cdr(record);
		}

		return false;
	}

	function insert() {
		// Keep retrieving tables at a given key until we hit 2 base cases

		// case keySet length === 1:
			//	add new record at table
		// case keySet length > 1
			// keep adding new tables until we hit keySet length === 1
	}

	return (operation, keys, val) => {
		switch (operation) {
			case "lookup":
				return lookup(keys);
			case "insert":
				insert(keys, val);
				break;
			case "print":
				print();
				break;
			default:
				throw "Operation not supported";
		}
	}
}











