/* Token values for Minnie lexer */

/* All tokens that have attributes have an attribute that is a string. */

typedef char* YYSTYPE;

/* Note: the following characters are tokens, and you can use the
   character itself as the token.

   '('
   ')'
   '['
   ']'
   ','
   '|'
   '='
   ':'
   ';'
   '\\'  (a backslash character)
*/


#define tok_and			257	/* Reserved word and 	*/
#define tok_bool		258	/* Reserved word bool 	*/
#define tok_case		259	/* Reserved word case 	*/
#define tok_char		260	/* Reserved word char	*/
#define tok_def			261	/* Reserved word def	*/
#define tok_else		262	/* Reserved word else	*/
#define tok_end			263	/* Reserved word end	*/
#define tok_evaluate		264	/* Reserved word evaluate */
#define tok_false		265	/* Reserved word false	*/
#define tok_in			266	/* Reserved word in	*/
#define tok_int			267	/* Reserved word int	*/
#define tok_let			268	/* Reserved word let	*/
#define tok_list		269	/* Reserved word list	*/
#define tok_maybe		270	/* Reserved word maybe	*/
#define tok_not			271	/* Reserved word not	*/
#define tok_or			272	/* Reserved word or	*/
#define tok_return		273	/* Reserved word return */
#define tok_state		274	/* Reserved word state	*/
#define tok_true		275	/* Reserved word true	*/
#define tok_type		276	/* Reserved word type	*/
#define tok_charconst		277	/* A character constant */
#define tok_intconst		278	/* An integer constant	*/
#define tok_stringconst		279	/* A string constant	*/
#define tok_id			280	/* An identifier	*/
#define tok_sglrightarrow	281	/* Token ->		*/
#define tok_dblrightarrow	282	/* Token =>		*/
#define tok_sglleftarrow	283	/* Token <-		*/
#define tok_dblcolon		284	/* Token ::		*/
#define tok_dblplus		285	/* Token ++		*/
#define tok_addop		286	/* Tokens +, -		*/
#define tok_mulop		287	/* Tokens *, /		*/
#define tok_relop		288	/* Tokens ==, <		*/
