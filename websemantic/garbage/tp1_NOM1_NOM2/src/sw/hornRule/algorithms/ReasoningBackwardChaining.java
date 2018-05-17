/**
 * 
 */
package sw.hornRule.algorithms;

import sw.hornRule.models.Formalism;

/**
 * @author  <Your name>
 *
 */
public class ReasoningBackwardChaining extends AlogrithmChaining {
 

	public boolean entailment(Formalism ruleBase, Formalism factBase, Formalism query) {
		return backwardChaining(ruleBase,factBase,query);
	}

	private boolean backwardChaining(Formalism ruleBase, Formalism factBase,
			Formalism query) {
		// TODO  To complete
		return false;
	}

	@Override
	public int countNbMatches() {
		// TODO To complete
		return 0;
	}

}
