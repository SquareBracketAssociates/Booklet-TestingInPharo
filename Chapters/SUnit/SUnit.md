## SUnit: The framework
	empty remove: 5.

	self should: [ empty remove: 5 ] raise: NotFound

	self skip.
	...

	true ifTrue: [^ self ].
	...
	"I'm tagging this expected failure because I'm not sure now if it is appropriate to keep 
	 or not with the removal of the 'traits-as-multiple-inheritance' stuff."
	<expectedFailure>

	self queryFromScope: ClyClassScope of: ClyClassWithTraits.
	self assert: resultItems size equals: 0