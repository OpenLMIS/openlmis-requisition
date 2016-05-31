package org.openlmis.restdocs;

import org.springframework.restdocs.RestDocumentation;

import java.io.*;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DocumentationUtility
{
    private static final String outputDirectory = "build/generated-snippets";

    public static RestDocumentation getRestDocumentation()
    {
        return new RestDocumentation(outputDirectory);
    }

    public static void writeClassProperties(String className, Map<String, List<String>> propertyMap) throws IOException
    {
        String outFile = outputDirectory + File.separator + className + "Properties.adoc";
        createFile(outFile);
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), "utf-8")))
        {
            writer.write("----" + System.lineSeparator());
            for (Map.Entry<String, List<String>> entry : propertyMap.entrySet())
            {
                String propertyName = entry.getKey();

                List<String> values = entry.getValue();
                String propertyType = values.get(0);
                propertyType = propertyType.substring(propertyType.lastIndexOf('.')+1);

                String output = propertyName + " (" + propertyType + ")";
                if(values.size() > 1)
                {
                    String propertyDescription = values.get(1);
                    output += " - " + propertyDescription;
                }

                writer.write(output +  System.lineSeparator() + System.lineSeparator() );
            }
            writer.write("----" + System.lineSeparator());
        }
    }


    public static Map<String, List<String>> getClassProperties(Class<?> theClass)
    {
        Map<String, List<String>> classInfo = new HashMap<String, List<String>>();
        Field[] fields = theClass.getDeclaredFields();
        for(Field field:fields)
        {
            ArrayList<String> metaProperties = new ArrayList<String>();
            metaProperties.add(field.getType().getName());
            classInfo.put(field.getName(), metaProperties);
        }
        return classInfo;
    }

    private static void createFile(String path) throws IOException
    {
        File f = new File(path);
        if (f.getParentFile() != null) {
            f.getParentFile().mkdirs();
        }
        f.createNewFile();
    }

}
