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




