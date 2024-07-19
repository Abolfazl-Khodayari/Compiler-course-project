package main.visitor.type;

import main.ast.nodes.Program;
import main.ast.nodes.declaration.*;
import main.ast.nodes.expression.*;
import main.ast.nodes.expression.operators.*;
import main.ast.nodes.expression.value.*;
import main.ast.nodes.expression.value.primitive.*;
import main.ast.nodes.statement.*;
import main.ast.type.*;
import main.ast.type.primitiveType.*;
import main.compileError.CompileError;
import main.compileError.typeErrors.*;
import main.symbolTable.SymbolTable;
import main.symbolTable.exceptions.*;
import main.symbolTable.item.*;
import main.visitor.Visitor;

import java.util.*;

public class TypeChecker extends Visitor<Type> {
    public ArrayList<CompileError> typeErrors = new ArrayList<>();
    @Override
    public Type visit(Program program){
        SymbolTable.root = new SymbolTable();
        SymbolTable.top = new SymbolTable();
        for(FunctionDeclaration functionDeclaration : program.getFunctionDeclarations()){
            FunctionItem functionItem = new FunctionItem(functionDeclaration);
            try {
                SymbolTable.root.put(functionItem);
            }catch (ItemAlreadyExists ignored){}
        }
        for(PatternDeclaration patternDeclaration : program.getPatternDeclarations()){
            PatternItem patternItem = new PatternItem(patternDeclaration);
            try{
                SymbolTable.root.put(patternItem);
            }catch (ItemAlreadyExists ignored){}
        }
        program.getMain().accept(this);

        return null;
    }
    @Override
    public Type visit(FunctionDeclaration functionDeclaration){
        SymbolTable.push(new SymbolTable());
            FunctionItem functionItem = (FunctionItem) SymbolTable.root.getItem(FunctionItem.START_KEY +
                    functionDeclaration.getFunctionName().getName());
            ArrayList<Type> currentArgTypes = functionItem.getArgumentTypes();
            for (int i = 0; i < functionDeclaration.getArgs().size(); i++) {
                VarItem argItem = new VarItem(functionDeclaration.getArgs().get(i).getName());
                argItem.setType(currentArgTypes.get(i));
                try {
                    SymbolTable.top.put(argItem);
                }catch (ItemAlreadyExists ignored){}
            }
        for(Statement statement : functionDeclaration.getBody())
            statement.accept(this);

        //TODO:Figure out whether return types of functions are not the same.
        SymbolTable.pop();
        return null;
        //TODO:Return the infered type of the function
    }
    @Override
    public Type visit(PatternDeclaration patternDeclaration){
        SymbolTable.push(new SymbolTable());
            PatternItem patternItem = (PatternItem) SymbolTable.root.getItem(PatternItem.START_KEY +
                    patternDeclaration.getPatternName().getName());
            VarItem varItem = new VarItem(patternDeclaration.getTargetVariable());
            varItem.setType(patternItem.getTargetVarType());
            try {
                SymbolTable.top.put(varItem);
            }catch (ItemAlreadyExists ignored){}
            for(Expression expression : patternDeclaration.getConditions()){
                if(!(expression.accept(this) instanceof BoolType)){
                    typeErrors.add(new ConditionIsNotBool(expression.getLine()));
                    SymbolTable.pop();
                    return new NoType();
                }
            }
            //TODO:1-figure out whether return expression of different cases in pattern are of the same type/2-return the infered type



        SymbolTable.pop();
        return null;
    }
    @Override
    public Type visit(MainDeclaration mainDeclaration){
        for (var stmt: mainDeclaration.getBody())
            stmt.accept(this);
        return null;
    }
    @Override
    public Type visit(AccessExpression accessExpression){
        if(accessExpression.isFunctionCall()){
            //TODO:function is called here.set the arguments type and visit its declaration
        }
        else{
            Type accessedType = accessExpression.getAccessedExpression().accept(this);
            if(!(accessedType instanceof StringType) && !(accessedType instanceof ListType)){
                typeErrors.add(new IsNotIndexable(accessExpression.getLine()));
                return new NoType();
            }
            //TODO:index of access list must be int
            return accessedType;
        }
        return null;
    }

    @Override
    public Type visit(ReturnStatement returnStatement){
        // TODO:Visit return statement.Note that return type of functions are specified here
        return null;
    }
    @Override
    public Type visit(ExpressionStatement expressionStatement){
        return expressionStatement.getExpression().accept(this);

    }
    @Override
    public Type visit(ForStatement forStatement){
        SymbolTable.push(SymbolTable.top.copy());
        forStatement.getRangeExpression().accept(this);
        VarItem varItem = new VarItem(forStatement.getIteratorId());
        try{
            SymbolTable.top.put(varItem);
        }catch (ItemAlreadyExists ignored){}

        for(Statement statement : forStatement.getLoopBodyStmts())
            statement.accept(this);
        SymbolTable.pop();
        return null;
    }
    @Override
    public Type visit(IfStatement ifStatement){
        SymbolTable.push(SymbolTable.top.copy());
        for(Expression expression : ifStatement.getConditions())
            if(!(expression.accept(this) instanceof BoolType))
                typeErrors.add(new ConditionIsNotBool(expression.getLine()));
        for(Statement statement : ifStatement.getThenBody())
            statement.accept(this);
        for(Statement statement : ifStatement.getElseBody())
            statement.accept(this);
        SymbolTable.pop();
        return new NoType();
    }
    @Override
    public Type visit(LoopDoStatement loopDoStatement){
        SymbolTable.push(SymbolTable.top.copy());
        for(Statement statement : loopDoStatement.getLoopBodyStmts())
            statement.accept(this);
        SymbolTable.pop();
        return new NoType();
    }
    @Override
    public Type visit(AssignStatement assignStatement){
        if(assignStatement.isAccessList()){
            // TODO:assignment to list
        }
        else{
            VarItem newVarItem = new VarItem(assignStatement.getAssignedId());
            Type exprType = assignStatement.getAssignExpression().accept(this);
            //assignStatement.ass
            //assignStatement.getAccessListExpression().accept(this);
            newVarItem.setType(exprType);
            // TODO:maybe new type for a variable
            try {
                SymbolTable.top.put(newVarItem);
            }catch (ItemAlreadyExists ignored){}
        }
        return new NoType();
    }
    @Override
    public Type visit(BreakStatement breakStatement){
        for(Expression expression : breakStatement.getConditions())
            if(!((expression.accept(this)) instanceof BoolType))
                typeErrors.add(new ConditionIsNotBool(expression.getLine()));

        return null;
    }
    @Override
    public Type visit(NextStatement nextStatement){
        for(Expression expression : nextStatement.getConditions())
            if(!((expression.accept(this)) instanceof BoolType))
                typeErrors.add(new ConditionIsNotBool(expression.getLine()));

        return null;
    }
    @Override
    public Type visit(PushStatement pushStatement){
        //TODO:visit push statement

        return new NoType();
    }
    @Override
    public Type visit(PutStatement putStatement){
        //TODO:visit putStatement

        return new NoType();

    }
    @Override
    public Type visit(BoolValue boolValue){
        return new BoolType();
    }
    @Override
    public Type visit(IntValue intValue){
        return new IntType();
    }
    @Override
    public Type visit(FloatValue floatValue){return new FloatType();}
    @Override
    public Type visit(StringValue stringValue){
        return new StringType();
    }
    @Override
    public Type visit(ListValue listValue){
        var a = listValue.getElements().getFirst().accept(this);
        Type atype = a;

        // TODO:visit listValue
        return atype;
    }
    @Override
    public Type visit(FunctionPointer functionPointer){
        return new FptrType(functionPointer.getId().getName());
    }
    @Override
    public Type visit(AppendExpression appendExpression){
        Type appendeeType = appendExpression.getAppendee().accept(this);
        if(!(appendeeType instanceof ListType) && !(appendeeType instanceof StringType)){
            typeErrors.add(new IsNotAppendable(appendExpression.getLine()));
            return new NoType();
        }
        if(appendeeType instanceof StringType){
            for(var exp : appendExpression.getAppendeds() )
                if(!(exp.accept(this) instanceof StringType)){
                    typeErrors.add(new AppendTypesMisMatch(appendExpression.getLine()));
                    return new NoType();
                }
        }
        if(appendeeType instanceof ListType){
            var type2 = ((ListType) appendeeType).getType();
            for(var exp : appendExpression.getAppendeds() )
                if(!(exp.accept(this).sameType(type2))){
                    typeErrors.add(new AppendTypesMisMatch(appendExpression.getLine()));
                    return new NoType();
                }
        }
        return appendeeType;
    }
    @Override
    public Type visit(BinaryExpression binaryExpression){
        //TODO:visit binary expression
        var type1 = binaryExpression.getFirstOperand().accept(this);
        var type2 = binaryExpression.getSecondOperand().accept(this);
        if(!(type1.sameType(type2))){
            typeErrors.add(new NonSameOperands(binaryExpression.getLine(), binaryExpression.getOperator()));
            return new NoType();
        }
        if(!(type1 instanceof IntType || type1 instanceof FloatType)){
            typeErrors.add(new UnsupportedOperandType(binaryExpression.getLine(), binaryExpression.getOperator().toString()));
            return new NoType();
        }
        return type1;
    }
    @Override
    public Type visit(UnaryExpression unaryExpression){
        //TODO:visit unaryExpression
        var operantype1 = unaryExpression.getExpression().accept(this);
        var operator1 = unaryExpression.getOperator().toString();
        if(operator1 == UnaryOperator.NOT.toString() && operantype1 instanceof BoolType){
            return new BoolType();
        }
        if( operator1 == UnaryOperator.NOT.toString() ){
            typeErrors.add(new UnsupportedOperandType(unaryExpression.getLine(), operator1));
            return new NoType();
        }
        if(!(operantype1 instanceof IntType || operantype1 instanceof FloatType)){
            typeErrors.add(new UnsupportedOperandType(unaryExpression.getLine(), operator1));
            return new NoType();
        }
        return operantype1;
    }
    @Override
    public Type visit(ChompStatement chompStatement){
        if (!(chompStatement.getChompExpression().accept(this) instanceof StringType)) {
            typeErrors.add(new ChompArgumentTypeMisMatch(chompStatement.getLine()));
            return new NoType();
        }

        return new StringType();
    }
    @Override
    public Type visit(ChopStatement chopStatement){
        Expression arg = chopStatement.getChopExpression();
        Type argType = arg.accept(this);
        if (!(argType instanceof StringType)) {
            typeErrors.add(new ChopArgumentTypeMisMatch(chopStatement.getLine()));
            return new NoType();
        }
        return new StringType();
    }
    @Override
    public Type visit(Identifier identifier){
        // TODO:visit Identifier
        String varName = identifier.getName();
        VarItem newVarItem2 = (VarItem) SymbolTable.top.getItem("VAR:" + varName);
        return newVarItem2.getType();
    }
    @Override
    public Type visit(LenStatement lenStatement){
        //TODO:visit LenStatement.Be carefull about the return type of LenStatement.
        return null;
    }
    @Override
    public Type visit(MatchPatternStatement matchPatternStatement){
            PatternItem patternItem = (PatternItem)SymbolTable.root.getItem(PatternItem.START_KEY +
                    matchPatternStatement.getPatternId().getName());
            patternItem.setTargetVarType(matchPatternStatement.getMatchArgument().accept(this));
            return patternItem.getPatternDeclaration().accept(this);
    }
    @Override
    public Type visit(RangeExpression rangeExpression){
        RangeType rangeType = rangeExpression.getRangeType();
        if(rangeType.equals(RangeType.IDENTIFIER)){
            for(Expression expr1: rangeExpression.getRangeExpressions()) {
                Type argType = expr1.accept(this);
                if(argType != null){
                    if(!(argType instanceof ListType)){
                        typeErrors.add(new IsNotIterable(expr1.getLine()));
                        return new NoType();
                    }
                }
            }
        }
        if(rangeType.equals(RangeType.DOUBLE_DOT)){
            var typefirst = rangeExpression.getRangeExpressions().getFirst().accept(this);
            var typesecond = rangeExpression.getRangeExpressions().getLast().accept(this);
            if(!(typefirst instanceof IntType && typesecond instanceof IntType)){
                typeErrors.add(new RangeValuesMisMatch(rangeExpression.getLine()));
                return new NoType();
            }
        }
        return new NoType();
    }
}
