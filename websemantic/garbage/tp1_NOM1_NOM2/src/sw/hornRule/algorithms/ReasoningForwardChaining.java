/**
 * 
 */
package sw.hornRule.algorithms;

import sw.hornRule.models.FactBase;
import sw.hornRule.models.Formalism;

/**
 * @author  <Your name>
 *
 */
public class ReasoningForwardChaining extends AlogrithmChaining {
 
	/**
	 * @param a knowledge base kb (in a given formalism)
	 * @param facts (in a given formalism)
	 * @return forwardChaining(ruleBase,factBase), also called the saturation of ruleBase w.r.t. factBase, 
	 * mathematically it computes the minimal fix point of KB from facts)
	 */
	//It's your turn to implement the algorithm, including the methods match() and eval()
	public FactBase forwardChaining(Formalism ruleBase, Formalism factBase){
		
		return new FactBase();
	};
	

	public boolean entailment(Formalism ruleBase, Formalism factBase, Formalism query) {
		FactBase allInferredFacts = forwardChaining(ruleBase, factBase);
		if(allInferredFacts.getFact().contains(query))
			return true;
		else
			return false;
	}

	@Override
	//It's your turn to implement this method
	public int countNbMatches() {
		// TODO Auto-generated method stub
		return 0;
	}

}
