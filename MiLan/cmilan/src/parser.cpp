#include "parser.h"
#include <sstream>

//Выполняем синтаксический разбор блока program. Если во время разбора не обнаруживаем 
//никаких ошибок, то выводим последовательность команд стек-машины
void Parser::parse()
{
	program(); 
	if(!error_) {
		codegen_->flush();
	}
}

void Parser::program()
{
	mustBe(T_BEGIN);
	statementList();
	mustBe(T_END);
	codegen_->emit(STOP);
}

void Parser::statementList()
{
	//	  Если список операторов пуст, очередной лексемой будет одна из возможных "закрывающих скобок": END, OD, ELSE, FI.
	//	  В этом случае результатом разбора будет пустой блок (его список операторов равен null).
	//	  Если очередная лексема не входит в этот список, то ее мы считаем началом оператора и вызываем метод statement. 
	//    Признаком последнего оператора является отсутствие после оператора точки с запятой.
	if(see(T_END) || see(T_OD) || see(T_ELSE) || see(T_FI)) {
		return;
	}
	else {
		bool more = true;
		while(more) {
			statement();
			more = match(T_SEMICOLON);
		}
	}
}

void Parser::statement()
{
	// Если встречаем переменную, то запоминаем ее адрес или добавляем новую если не встретили. 
	// Следующей лексемой должно быть присваивание. Затем идет блок expression, который возвращает значение на вершину стека.
	// Записываем это значение по адресу нашей переменной
	if(see(T_IDENTIFIER)) {
		string ident = scanner_->getStringValue();
		next();
		if (match(T_LQPAREN)) {
			int address = findArray(ident);
			if (address == -1) {
				std::ostringstream msg;
				msg << "no such array: " << ident << ".";
				reportError(msg.str());
				recover(T_ASSIGN);
				expression();
			}
			else {
				expression();
				mustBe(T_RQPAREN);
				codegen_->emit(DUP); //дублируем значение индекса для сравнения с 0
				codegen_->emit(DUP); //дублируем значение индекса для сравнения с размером массива
				int sizeAddress = findSize(ident);
				codegen_->emit(LOAD, sizeAddress);
				codegen_->emit(COMPARE, 2);
				codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
				codegen_->emit(JUMP, -1);
				codegen_->emit(PUSH, 0);
				codegen_->emit(COMPARE, 5);
				codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
				codegen_->emit(JUMP, -1);
				codegen_->emit(STORE, reserveAddress_);
				mustBe(T_ASSIGN);
				expression();
				codegen_->emit(LOAD, reserveAddress_);
				codegen_->emit(BSTORE, address);
			}
		}
		else {
			int addr = findArray(ident);
			//необходимо определить имеем дело с переменной или с массивом
			if (addr == -1) {
				int varAddress = findOrAddVariable(ident);
				mustBe(T_ASSIGN);
				expression();
				codegen_->emit(STORE, varAddress);
			}
			else {
				mustBe(T_ASSIGN);
				if (match(T_LQPAREN)) {
					codegen_->emit(PUSH, 0);
					codegen_->emit(STORE, reserveAddress_); //хранит индекс первого массива
					codegen_->emit(PUSH, 0);
					codegen_->emit(STORE, reserveAddress_ + 1); //хранит вспомагательный индекс
					codegen_->emit(PUSH, 0);
					codegen_->emit(STORE, reserveAddress_ + 2); //хранит размер полученного массива
					int addr1, addr2;
					int size1, size2;
					if (see(T_IDENTIFIER)) {
						addr1 = findArray(scanner_->getStringValue());
						if (addr1 == -1) {
							reportError("the first argument must be array.");
						}
						else {
							size1 = findSize(scanner_->getStringValue());
						}
					}
					else {
						std::ostringstream msg;
						msg << "array identifier expected but found " << tokenToString(scanner_->token()) << '.';
						reportError(msg.str());
					}
					next();
					Arithmetic op;
					if (see(T_ARROP)) {
						op = scanner_->getArithmeticValue();
					}
					else {
						std::ostringstream msg;
						msg << tokenToString(T_ARROP) << " expected but found " << tokenToString(scanner_->token()) << '.';
						reportError(msg.str());
					}
					next();
					if (see(T_IDENTIFIER)) {
						addr2 = findArray(scanner_->getStringValue());
						if (addr2 == -1) {
							reportError("the second argument must be array.");
						}
						else {
							size2 = findSize(scanner_->getStringValue());
						}
					}
					else {
						std::ostringstream msg;
						msg << "array identifier expected but found " << tokenToString(scanner_->token()) << '.';
						reportError(msg.str());
					}
					next();
					mustBe(T_RQPAREN);
					if (op == A_PLUS) {
						orCode(addr1, size1);
						codegen_->emit(PUSH, 0);
						codegen_->emit(STORE, reserveAddress_);
						orCode(addr2, size2);
					}
					else {
						andCode(addr1, size1, addr2, size2);
					}
					copyToDest(addr, findSize(ident));
					clear();
				}
				else {
					codegen_->emit(PUSH, 0);
					codegen_->emit(STORE, reserveAddress_);
					codegen_->emit(LOAD, findSize(ident));
					codegen_->emit(STORE, reserveAddress_ + 1);
					int comandAddr = codegen_->getCurrentAddress();
					arrExpression();
					codegen_->emit(LOAD, reserveAddress_);
					codegen_->emit(PUSH, 1);
					codegen_->emit(ADD);
					codegen_->emit(DUP);
					codegen_->emit(STORE, reserveAddress_);
					codegen_->emit(LOAD, reserveAddress_ + 1);
					codegen_->emit(COMPARE, 2);
					codegen_->emit(JUMP_YES, comandAddr);
					codegen_->emit(LOAD, reserveAddress_);
					codegen_->emit(PUSH, 1);
					codegen_->emit(SUB);
					codegen_->emit(STORE, reserveAddress_);
					comandAddr = codegen_->getCurrentAddress();
					codegen_->emit(LOAD, reserveAddress_);
					codegen_->emit(BSTORE, addr);
					codegen_->emit(LOAD, reserveAddress_);
					codegen_->emit(PUSH, 1);
					codegen_->emit(SUB);
					codegen_->emit(DUP);
					codegen_->emit(STORE, reserveAddress_);
					codegen_->emit(PUSH, 0);
					codegen_->emit(COMPARE, 5);
					codegen_->emit(JUMP_YES, comandAddr);
				}
			}
		}
	}
	// Если встретили IF, то затем должно следовать условие. На вершине стека лежит 1 или 0 в зависимости от выполнения условия.
	// Затем зарезервируем место для условного перехода JUMP_NO к блоку ELSE (переход в случае ложного условия). Адрес перехода
	// станет известным только после того, как будет сгенерирован код для блока THEN.
	else if(match(T_IF)) {
		relation();
		
		int jumpNoAddress = codegen_->reserve();

		mustBe(T_THEN);
		statementList();
		if(match(T_ELSE)) {
		//Если есть блок ELSE, то чтобы не выполнять его в случае выполнения THEN, 
		//зарезервируем место для команды JUMP в конец этого блока
			int jumpAddress = codegen_->reserve();
		//Заполним зарезервированное место после проверки условия инструкцией перехода в начало блока ELSE.
			codegen_->emitAt(jumpNoAddress, JUMP_NO, codegen_->getCurrentAddress());
			statementList();
		//Заполним второй адрес инструкцией перехода в конец условного блока ELSE.
			codegen_->emitAt(jumpAddress, JUMP, codegen_->getCurrentAddress());
		}
		else {
		//Если блок ELSE отсутствует, то в зарезервированный адрес после проверки условия будет записана
		//инструкция условного перехода в конец оператора IF...THEN
			codegen_->emitAt(jumpNoAddress, JUMP_NO, codegen_->getCurrentAddress());
		}

		mustBe(T_FI);
	}

	else if(match(T_WHILE)) {
		//запоминаем адрес начала проверки условия.
		int conditionAddress = codegen_->getCurrentAddress();
		relation();
		//резервируем место под инструкцию условного перехода для выхода из цикла.
		int jumpNoAddress = codegen_->reserve();
		mustBe(T_DO);
		statementList();
		mustBe(T_OD);
		//переходим по адресу проверки условия
		codegen_->emit(JUMP, conditionAddress);
		//заполняем зарезервированный адрес инструкцией условного перехода на следующий за циклом оператор.
		codegen_->emitAt(jumpNoAddress, JUMP_NO, codegen_->getCurrentAddress());
	}
	else if(match(T_WRITE)) {
		mustBe(T_LPAREN);
		expression();
		mustBe(T_RPAREN);
		codegen_->emit(PRINT);
	}
	else if (match(T_ARRAY)) {
		string ident = ""; //имя массива
		int size = 0; //размер массива
		if (!see(T_IDENTIFIER)) { //после ключевого слова array должен следовать идентификатор - имя массива
			reportError("identifier expected.");
		}
		else {
			ident = scanner_->getStringValue();
			if (findVariable(ident) != -1) {
				reportError("variable with such name already exists.");
			}
		}
		next();
		mustBe(T_LQPAREN);
		if (!see(T_NUMBER) || scanner_->getIntValue() <= 0) { //размер массива в [], должен быть больше 0
			reportError("positive number expected.");
			size = 1;
		}
		else {
			size = scanner_->getIntValue();
		}
		next();
		mustBe(T_RQPAREN);
		if (!ident.empty() && size != 0) {
			int arrAddress = addArray(ident, size);
			if (arrAddress < 0) {
				reportError("redefining an existing array.");
			}
			else {
				int addr = findSize(ident);
				codegen_->emit(PUSH, size);
				codegen_->emit(STORE, addr);
			}
		}
	}
	else if (match(T_DELETE)) {
		match(T_LPAREN);
		if (see(T_IDENTIFIER)) {
			string ident = scanner_->getStringValue();
			int address = findArray(ident);
			if (address == -1) {
				std::ostringstream msg;
				msg << "Unknown array " << ident << '.';
				reportError(msg.str());
			}
			next();
			match(T_RPAREN);
			int sizeAddress = findSize(ident);
			codegen_->emit(LOAD, sizeAddress);
			codegen_->emit(DUP);
			codegen_->emit(PUSH, 0);
			codegen_->emit(COMPARE, 3);
			codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
			codegen_->emit(JUMP, -1);
			codegen_->emit(PUSH, 1);
			codegen_->emit(SUB);
			codegen_->emit(STORE, sizeAddress);
			codegen_->emit(PUSH, 0);
			codegen_->emit(LOAD, sizeAddress);
			codegen_->emit(BSTORE, address);
		}
	}
	else {
		reportError("statement expected.");
	}
}

void Parser::expression()
{

	 /*
         Арифметическое выражение описывается следующими правилами: <expression> -> <term> | <term> + <term> | <term> - <term>
         При разборе сначала смотрим первый терм, затем анализируем очередной символ. Если это '+' или '-', 
		 удаляем его из потока и разбираем очередное слагаемое (вычитаемое). Повторяем проверку и разбор очередного 
		 терма, пока не встретим за термом символ, отличный от '+' и '-'
     */

	term();
	while(see(T_ADDOP)) {
		Arithmetic op = scanner_->getArithmeticValue();
		next();
		term();

		if(op == A_PLUS) {
			codegen_->emit(ADD);
		}
		else {
			codegen_->emit(SUB);
		}
	}
}

void Parser::term()
{
	 /*  
		 Терм описывается следующими правилами: <term> -> <factor> | <factor> * <factor> | <factor> / <factor>
         При разборе сначала смотрим первый множитель, затем анализируем очередной символ. Если это '*' или '/', 
		 удаляем его из потока и разбираем очередное слагаемое (вычитаемое). Повторяем проверку и разбор очередного 
		 множителя, пока не встретим за ним символ, отличный от '*' и '/' 
	*/
	factor();
	while(see(T_MULOP)) {
		Arithmetic op = scanner_->getArithmeticValue();
		next();
		factor();

		if(op == A_MULTIPLY) {
			codegen_->emit(MULT);
		}
		else {
			codegen_->emit(DIV);
		}
	}
}

void Parser::factor()
{
	/*
		Множитель описывается следующими правилами:
		<factor> -> number | identifier | -<factor> | (<expression>) | READ
	*/
	if(see(T_NUMBER)) {
		int value = scanner_->getIntValue();
		next();
		codegen_->emit(PUSH, value);
		//Если встретили число, то преобразуем его в целое и записываем на вершину стека
	}
	else if(see(T_IDENTIFIER)) {
		string ident = scanner_->getStringValue();
		next();
		if (match(T_LQPAREN)) {
			int address = findArray(ident);
			if (address == -1) {
				std::ostringstream msg;
				msg << "no such array: " << ident << ".";
				reportError(msg.str());
			}
			else {
				expression();
				mustBe(T_RQPAREN);
				codegen_->emit(DUP);
				codegen_->emit(DUP);
				int sizeAddress = findSize(ident);
				codegen_->emit(LOAD, sizeAddress);
				codegen_->emit(COMPARE, 2);
				codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
				codegen_->emit(JUMP, -1);
				codegen_->emit(PUSH, 0);
				codegen_->emit(COMPARE, 5);
				codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
				codegen_->emit(JUMP, -1);
				codegen_->emit(BLOAD, address);
			}
		}
		else {
			int isArr = findArray(ident);
			if (isArr != -1) {
				std::ostringstream msg;
				msg << "inappropriate use of array: " << ident << ".";
				reportError(msg.str());
			}
			int varAddress = findOrAddVariable(ident);
			codegen_->emit(LOAD, varAddress);
			//Если встретили переменную, то выгружаем значение, лежащее по ее адресу, на вершину стека 
		}
	}
	else if(see(T_ADDOP) && scanner_->getArithmeticValue() == A_MINUS) {
		next();
		factor();
		codegen_->emit(INVERT);
		//Если встретили знак "-", и за ним <factor> то инвертируем значение, лежащее на вершине стека
	}
	else if(match(T_LPAREN)) {
		expression();
		mustBe(T_RPAREN);
		//Если встретили открывающую скобку, тогда следом может идти любое арифметическое выражение и обязательно
		//закрывающая скобка.
	}
	else if(match(T_READ)) {
		codegen_->emit(INPUT);
		//Если встретили зарезервированное слово READ, то записываем на вершину стека идет запись со стандартного ввода
	}
	else {
		reportError("expression expected.");
	}
}

void Parser::relation()
{
	//Условие сравнивает два выражения по какому-либо из знаков. Каждый знак имеет свой номер. В зависимости от 
	//результата сравнения на вершине стека окажется 0 или 1.
	expression();
	if(see(T_CMP)) {
		Cmp cmp = scanner_->getCmpValue();
		next();
		expression();
		switch(cmp) {
			//для знака "=" - номер 0
			case C_EQ:
				codegen_->emit(COMPARE, 0);
				break;
			//для знака "!=" - номер 1
			case C_NE:
				codegen_->emit(COMPARE, 1);
				break;
			//для знака "<" - номер 2
			case C_LT:
				codegen_->emit(COMPARE, 2);
				break;
			//для знака ">" - номер 3
			case C_GT:
				codegen_->emit(COMPARE, 3);
				break;
			//для знака "<=" - номер 4
			case C_LE:
				codegen_->emit(COMPARE, 4);
				break;
			//для знака ">=" - номер 5
			case C_GE:
				codegen_->emit(COMPARE, 5);
				break;
		};
	}
	else {
		reportError("comparison operator expected.");
	}
}

void Parser::arrExpression() {
	arrTerm();
	while (see(T_ADDOP)) {
		Arithmetic op = scanner_->getArithmeticValue();
		next();
		arrTerm();
		if (op == A_PLUS) {
			codegen_->emit(ADD);
		}
		else {
			codegen_->emit(SUB);
		}
	}
}

void Parser::arrTerm() {
	arrFactor();
	while (see(T_MULOP)) {
		Arithmetic op = scanner_->getArithmeticValue();
		next();
		arrFactor();
		if (op == A_MULTIPLY) {
			codegen_->emit(MULT);
		}
		else {
			codegen_->emit(DIV);
		}
	}
}

void Parser::arrFactor() {
	if (see(T_IDENTIFIER)) {
		int arrAddress = findArray(scanner_->getStringValue());
		if (arrAddress == -1) {
			std::ostringstream msg;
			msg << "Unknown array " << scanner_->getStringValue();
			reportError(msg.str());
		}
		else {
			codegen_->emit(LOAD, reserveAddress_ + 1);
			codegen_->emit(LOAD, findSize(scanner_->getStringValue()));
			codegen_->emit(COMPARE, 0);
			codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
			codegen_->emit(JUMP, -1);
			
			codegen_->emit(LOAD, reserveAddress_);
			codegen_->emit(BLOAD, arrAddress);
		}
		next();
	}
	else if (see(T_ADDOP) && scanner_->getArithmeticValue() == A_MINUS) {
		next();
		arrFactor();
		codegen_->emit(INVERT);
	}
	else if (match(T_LPAREN)) {
		arrExpression();
		mustBe(T_RPAREN);
	}
	else {
		reportError("Array expected.");
	}
}

int Parser::findOrAddVariable(const string& var)
{
	VarTable::iterator it = variables_.find(var);
	if(it == variables_.end()) {
		variables_[var] = lastVar_;
		return lastVar_++;
	}
	else {
		return it->second;
	}
}

int Parser::findVariable(const string& var)
{
	VarTable::iterator it = variables_.find(var);
	if (it == variables_.end()) {
		return -1;
	}
	else {
		return it->second;
	}
}

int Parser::findArray(const string& arr) {
	VarTable::iterator it = arrays_.find(arr);
	if (it == arrays_.end()) {
		return -1;
	}
	else {
		return it->second;
	}
}

int Parser::addArray(const string& arr, int offset)
{
	VarTable::iterator it = arrays_.find(arr);
	if (it == arrays_.end()) {
		arrays_[arr] = lastVar_;
		int oldLastVar_ = lastVar_;
		lastVar_ += offset;
		arraySizes_[arr] = lastVar_++;
		return oldLastVar_;
	}
	else {
		return -1; //нельзя переопределять размер массива
	}
}

int Parser::findSize(const string& var)
{
	VarTable::iterator it = arraySizes_.find(var);
	if (it == arraySizes_.end()) {
		return -1;
	}
	else {
		return it->second;
	}
}

void Parser::mustBe(Token t)
{
	if(!match(t)) {
		error_ = true;

		// Подготовим сообщение об ошибке
		std::ostringstream msg;
		msg << tokenToString(scanner_->token()) << " found while " << tokenToString(t) << " expected.";
		reportError(msg.str());

		// Попытка восстановления после ошибки.
		recover(t);
	}
}

void Parser::recover(Token t, bool goToNext)
{
	while (!see(t) && !see(T_EOF)) {
		next();
	}

	if (see(t)) {
		next();
	}
}

void Parser::orCode(int arrAddress, int sizeAddress) 
{
	codegen_->emit(LOAD, 0);
	codegen_->emit(BLOAD, arrAddress);
	codegen_->emit(LOAD, 2);
	codegen_->emit(PUSH, 0);
	codegen_->emit(COMPARE, 0);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 18);
	codegen_->emit(PUSH, 0);
	codegen_->emit(STORE, 1);
	codegen_->emit(DUP);
	codegen_->emit(LOAD, 1);
	codegen_->emit(BLOAD, lastVar_);
	codegen_->emit(COMPARE, 0);
	codegen_->emit(JUMP_NO, codegen_->getCurrentAddress() + 3);
	codegen_->emit(POP);
	codegen_->emit(JUMP, codegen_->getCurrentAddress() + 15);
	codegen_->emit(LOAD, 1);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 1);
	codegen_->emit(LOAD, 2);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 14);
	codegen_->emit(LOAD, 2);
	codegen_->emit(BSTORE, lastVar_);
	codegen_->emit(LOAD, 2);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(STORE, 2);
	codegen_->emit(LOAD, 0);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 0);
	codegen_->emit(LOAD, sizeAddress);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 36);
}

void Parser::andCode(int arrAddress1, int sizeAddress1, int arrAddress2, int sizeAddress2) {
	codegen_->emit(LOAD, 0);
	codegen_->emit(BLOAD, arrAddress1);
	codegen_->emit(PUSH, 0);
	codegen_->emit(STORE, 1);
	codegen_->emit(DUP);
	codegen_->emit(LOAD, 1);
	codegen_->emit(BLOAD, arrAddress2);
	codegen_->emit(COMPARE, 0);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 11);
	codegen_->emit(LOAD, 1);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 1);
	codegen_->emit(LOAD, sizeAddress2);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 12);
	codegen_->emit(POP);
	codegen_->emit(JUMP, codegen_->getCurrentAddress() + 28);
	codegen_->emit(LOAD, 2);
	codegen_->emit(PUSH, 0);
	codegen_->emit(COMPARE, 0);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 18);
	codegen_->emit(PUSH, 0);
	codegen_->emit(STORE, 1);
	codegen_->emit(DUP);
	codegen_->emit(LOAD, 1);
	codegen_->emit(BLOAD, lastVar_);
	codegen_->emit(COMPARE, 0);
	codegen_->emit(JUMP_NO, codegen_->getCurrentAddress() + 3);
	codegen_->emit(POP);
	codegen_->emit(JUMP, codegen_->getCurrentAddress() + 15);
	codegen_->emit(LOAD, 1);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 1);
	codegen_->emit(LOAD, 2);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 14);
	codegen_->emit(LOAD, 2);
	codegen_->emit(BSTORE, lastVar_);
	codegen_->emit(LOAD, 2);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(STORE, 2);
	codegen_->emit(LOAD, 0);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 0);
	codegen_->emit(LOAD, sizeAddress1);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 53);
}

void Parser::clear()
{
	//index:=0
	codegen_->emit(PUSH, 0);
	codegen_->emit(STORE, 1);
	//while index < size do mem[index]:=0; index++; done
	codegen_->emit(PUSH, 0);
	codegen_->emit(LOAD, 1);
	codegen_->emit(BSTORE, lastVar_);
	codegen_->emit(LOAD, 1);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 1);
	codegen_->emit(LOAD, 2);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 10);
}

void Parser::copyToDest(int address, int size) {
	//if size <= arraySize then OK else JUMP -1
	codegen_->emit(LOAD, 2);
	codegen_->emit(LOAD, size);
	codegen_->emit(COMPARE, 4);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() + 2);
	codegen_->emit(JUMP, -1);
	//index:=0
	codegen_->emit(PUSH, 0);
	codegen_->emit(STORE, 1);
	//while index < size do array[index]:=mem[index];index++; done
	codegen_->emit(LOAD, 1);
	codegen_->emit(BLOAD, lastVar_);
	codegen_->emit(LOAD, 1);
	codegen_->emit(BSTORE, address);
	codegen_->emit(LOAD, 1);
	codegen_->emit(PUSH, 1);
	codegen_->emit(ADD);
	codegen_->emit(DUP);
	codegen_->emit(STORE, 1);
	codegen_->emit(LOAD, 2);
	codegen_->emit(COMPARE, 2);
	codegen_->emit(JUMP_YES, codegen_->getCurrentAddress() - 11);
}