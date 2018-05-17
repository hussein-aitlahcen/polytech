/**
 * 
 */
package sw.hornRule.algorithms;

import sw.hornRule.models.Formalism;

/**
 * @author  <Your name>
 *
 */
public class ReasoningBackwardChainingwithQuestions extends AlogrithmChaining {

	@Override
	public boolean entailment(Formalism ruleBase, Formalism factBase, Formalism Query) {
		// TODO To complete
		// When a literal (i.e. a variable or its negation) cannot be replied by deductive reasoning, 
		// it will be asked to users to give an answer (if the liter holds according to the user)
		return false;
	}
 

	@Override
	public int countNbMatches() {
		// TODO To complete
		return 0;
	}

}
